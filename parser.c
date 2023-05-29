#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>


#include "parser.h"
#include "lexer.h"
#include "ad.h"
#include "utils.h"
#include "at.h"
// #include "at.c"

Symbol* owner;

Token* iTk; // the iterator in the tokens list
Token* consumedTk; // the last consumed token
bool expr();
bool exprPostfix();
bool stmCompound(bool newDomain);
bool stm();

void tkerr(const char* fmt, ...) {
    fprintf(stderr, "error in line %d: ", iTk->line);
    va_list va;
    va_start(va, fmt);
    vfprintf(stderr, fmt, va);
    va_end(va);
    fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
}

bool consume(int code) {
    if (iTk->code == code) {
        consumedTk = iTk;
        iTk = iTk->next;
        return true;
    }
    return false;
}

bool arrayDecl(Type* t) {
    Token* start = iTk;
    if (consume(LBRACKET)) {
        if (consume(INT)) {
            Token* tkSize = consumedTk;
            t->n = tkSize->i;
        }
        else
            t->n = 0;
        if (consume(RBRACKET)) {
            return true;
        }
        else
            tkerr("Missing ] or invalid expression inside [...]\n");
    }
    iTk = start;
    return false;
}
// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase(Type* t) {
    t->n = -1;
    Token* start = iTk;

    if (consume(TYPE_INT)) {
        t->tb = TB_INT;
        return true;
    }
    if (consume(TYPE_DOUBLE)) {
        t->tb = TB_DOUBLE;
        return true;
    }
    if (consume(TYPE_CHAR)) {
        t->tb = TB_CHAR;
        return true;
    }
    if (consume(STRUCT)) {
        if (consume(ID)) {
            Token* tkName = consumedTk;
            t->tb = TB_STRUCT;
            t->s = findSymbol(tkName->text);
            if (!t->s)tkerr("Structure undefined: %s", tkName->text);
            return true;
        }
    }
    iTk = start;
    return false;
}

bool varDef() {
    Type t;
    Token* start = iTk;
    if (typeBase(&t)) {
        if (consume(ID)) {
            Token* tkName = consumedTk;
            if (arrayDecl(&t)) {
                if (t.n == 0)tkerr("a vector variable must have a specified dimension");
            }

            if (consume(SEMICOLON)) {
                Symbol* var = findSymbolInDomain(symTable, tkName->text);
                if (var)tkerr("symbol redefinition: %s", tkName->text);
                var = newSymbol(tkName->text, SK_VAR);
                var->type = t;
                var->owner = owner;
                addSymbolToDomain(symTable, var);
                if (owner) {
                    switch (owner->kind) {
                    case SK_FN:
                        var->varIdx = symbolsLen(owner->fn.locals);
                        addSymbolToList(&owner->fn.locals, dupSymbol(var));
                        break;
                    case SK_STRUCT:
                        var->varIdx = typeSize(&owner->type);
                        addSymbolToList(&owner->structMembers, dupSymbol(var));
                        break;
                    }
                }
                else {
                    var->varMem = safeAlloc(typeSize(&t));
                }
                return true;
            }
            else
                tkerr("Missing ; after the variable declaration");
        }
        else
            tkerr("Missing variable name");
    }
    iTk = start;
    return false;
}


bool structDef() {
    Token* start = iTk;
    if (consume(STRUCT)) {
        if (consume(ID)) {
            Token* tkName = consumedTk;
            if (consume(LACC)) {
                Symbol* s = findSymbolInDomain(symTable, tkName->text);
                if (s)tkerr("symbol redefinition: %s", tkName->text);
                s = addSymbolToDomain(symTable, newSymbol(tkName->text, SK_STRUCT));
                s->type.tb = TB_STRUCT;
                s->type.s = s;
                s->type.n = -1;
                pushDomain();
                owner = s;
                for (;;) {
                    if (varDef()) {}
                    else {
                        break;
                    }
                }
                if (consume(RACC)) {
                    if (consume(SEMICOLON)) {
                        owner = NULL;
                        dropDomain;
                        return true;
                    }
                    else
                        tkerr("Missing ; after structure declaration");
                }
                else
                    tkerr("Missing } on structure declaration");
            }
        }
        else
            tkerr("Missing structure name after structure declaration");
    }
    iTk = start;
    return false;
}

bool exprUnary(Ret* r) {
    Token* start = iTk;
    if (consume(SUB)) {
        if (exprUnary(r)) {
            if (!canBeScalar(r))tkerr("unary - or ! must have a scalar operand");
            r->lval = false;
            r->ct = true;
            return true;
        }
        else
            tkerr("Invalid expression after - character");
    }
    if (consume(NOT)) {
        if (exprUnary(r)) {
            if (!canBeScalar(r))tkerr("unary - or ! must have a scalar operand");
            r->lval = false;
            r->ct = true;
            return true;
        }
        else
            tkerr("Invalid expression after ! character");
    }
    if (exprPostfix(r)) {
        return true;
    }
    iTk = start;
    return false;
}
bool exprCast(Ret* r) {
    Token* start = iTk;
    if (consume(LPAR)) {
        Ret op;
        Type t;
        if (typeBase(&t)) {
            if (arrayDecl(&t)) {}
            if (consume(RPAR)) {
                if (exprCast(&op)) {
                    if (t.tb == TB_STRUCT)tkerr("cannot convert to a struct type");
                    if (op.type.tb == TB_STRUCT)tkerr("cannot convert a struct");
                    if (op.type.n >= 0 && t.n < 0)tkerr("an array can be converted only to another array");
                    if (op.type.n < 0 && t.n >= 0)tkerr("a scalar can be converted only to another scalar");
                    *r = (Ret){ t,false,true };
                    return true;
                }
                else
                    tkerr("Invalid expression on cast operation");
            }
            else
                tkerr("Missing ) on cast operation");
        }
        else
            tkerr("Wrong type on cast operation");
    }
    else if (exprUnary(r)) {
        return true;
    }
    iTk = start;
    return false;
}

// exprMul: exprMul ( MUL | DIV ) exprCast | exprCast
//
// exprMul: exprCast exprMulPrim
// exprMulPrim:( MUL | DIV ) exprCast exprMulPrim
bool exprMulPrim(Ret* r) {
    Token* start = iTk;
    if (consume(MUL)) {
        Ret right;
        if (exprCast(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for * or /");
            *r = (Ret){ tDst,false,true };
            if (exprMulPrim(r)) {
                return true;
            }
            else
                tkerr("Invalid expression after cast operation");
        }
        else
            tkerr("Invalid expression after * operation");
    }
    if (consume(DIV)) {
        Ret right;
        if (exprCast(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for * or /");
            *r = (Ret){ tDst,false,true };
            if (exprMulPrim(r)) {
                return true;
            }
            else
                tkerr("Invalid expression after cast operation");
        }
        else
            tkerr("Invalid expression after / operation");
    }

    iTk = start;
    return true;
}
bool exprMul(Ret* r) {
    Token* start = iTk;
    if (exprCast(r)) {
        if (exprMulPrim(r)) {
            return true;
        }
        else
            tkerr("Invalid expression after cast operation");
    }
    iTk = start;
    return false;
}
// exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul
//
// exprAdd: exprMul exprAddPrim
// exprAddPrim: ( ADD | SUB ) exprMul exprAddPrim
bool exprAddPrim(Ret* r) {
    Token* start = iTk;
    if (consume(ADD)) {
        Ret right;
        if (exprMul(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for + or -");
            *r = (Ret){ tDst,false,true };
            if (exprAddPrim(r)) {
                return true;
            }
            else
                tkerr("Invalid expression after multiplication expression");
        }
        else
            tkerr("Invalid expression after + operation");
    }
    if (consume(SUB)) {
        Ret right;
        if (exprMul(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for + or -");
            *r = (Ret){ tDst,false,true };
            if (exprAddPrim(r)) {
                return true;
            }
            else
                tkerr("Invalid expression after multiplication expression");
        }
        else
            tkerr("Invalid expression after - operation");
    }

    iTk = start;
    return true;
}
bool exprAdd(Ret* r) {
    Token* start = iTk;
    if (exprMul(r)) {
        if (exprAddPrim(r)) {
            return true;
        }
        else
            tkerr("Invalid expression after multiplication expression");
    }
    iTk = start;
    return false;
}
// exprRel: exprRel ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd | exprAdd
//
// exprRel: exprAdd exprRelPrim
// exprRelPrim: ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd exprRelPrim
bool exprRelPrim(Ret* r) {
    Token* start = iTk;
    if (consume(LESS)) {
        Ret right;

        if (exprAdd(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for <, <=, >, >= ");
            *r = (Ret){ {TB_INT,NULL,-1},false,true };
            if (exprRelPrim(r)) {
                return true;
            }
            else
                tkerr("Invalid expression after add expression");
        }
        else
            tkerr("Invalid expression after < symbol");
    }
    if (consume(LESSEQ)) {
        Ret right;

        if (exprAdd(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for <, <=, >, >=");
            *r = (Ret){ {TB_INT,NULL,-1},false,true };
            if (exprRelPrim(r)) {
                return true;
            }
            else
                tkerr("Invalid expression after add expression");
        }
        else
            tkerr("Invalid expression after <= symbol");
    }
    if (consume(GREATER)) {
        Ret right;

        if (exprAdd(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for <, <=, >, >=");
            *r = (Ret){ {TB_INT,NULL,-1},false,true };
            if (exprRelPrim(r)) {
                return true;
            }
            else
                tkerr("Invalid expression after add expression");
        }
        else
            tkerr("Invalid expression after > symbol");
    }
    if (consume(GREATEREQ)) {
        Ret right;

        if (exprAdd(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for <, <=, >, >=");
            *r = (Ret){ {TB_INT,NULL,-1},false,true };
            if (exprRelPrim(r)) {
                return true;
            }
            else
                tkerr("Invalid expression after add expression");
        }
        else
            tkerr("Invalid expression after >= symbol");
    }

    iTk = start;
    return true;
}
bool exprRel(Ret* r) {
    Token* start = iTk;
    if (exprAdd(r)) {
        if (exprRelPrim(r)) {
            return true;
        }
        else
            tkerr("Invalid expression after add expression");
    }
    iTk = start;
    return false;
}
// exprEq: exprEq ( EQUAL | NOTEQ ) exprRel | exprRel
//
// exprEq: exprRel exprEqPrim
// exprEqPrim: ( EQUAL | NOTEQ ) exprRel exprEqPrim
bool exprEqPrim(Ret* r) {
    Token* start = iTk;
    if (consume(EQUAL)) {
        Ret right;
        if (exprRel(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for == or != ");
            *r = (Ret){ {TB_INT,NULL,-1},false,true };

            if (exprEqPrim(r)) {
                return true;
            }
            else
                tkerr("Invalid expression after rel expression");
        }
        else
            tkerr("Invalid expression after == symbol");
    }
    if (consume(NOTEQ)) {
        Ret right;
        if (exprRel(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for <, <=, >, >=");
            *r = (Ret){ {TB_INT,NULL,-1},false,true };
            if (exprEqPrim(r)) {
                return true;
            }
            else
                tkerr("Invalid expression after rel expression");
        }
        else
            tkerr("Invalid expression after != symbol");
    }

    iTk = start;
    return true;
}
bool exprEq(Ret* r) {
    Token* start = iTk;
    if (exprRel(r)) {
        if (exprEqPrim(r)) {
            return true;
        }
        else
            tkerr("Invalid expression after rel expression");
    }
    iTk = start;
    return false;
}
// exprAnd: exprAnd AND exprEq | exprEq
//
// exprAnd: exprEq exprAndPrim
// exprAndPrim: AND exprEq exprAndPrim | epsilon

bool exprAndPrim(Ret* r) {
    Token* start = iTk;
    if (consume(AND)) {
        Ret right;
        if (exprEq(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for &&");
            *r = (Ret){ {TB_INT,NULL,-1},false,true };

            if (exprAndPrim(r)) {
                return true;
            }
            else
                tkerr("Invalid expression after eq expression");
        }
        else
            tkerr("Invalid expression after && operator");
    }

    iTk = start;
    return true;
}
bool exprAnd(Ret* r) {
    Token* start = iTk;
    if (exprEq(r)) {
        if (exprAndPrim(r)) {
            return true;
        }
        else
            tkerr("Invalid expression after eq expression");
    }
    iTk = start;
    return false;
}

// exprOr: exprOr OR exprAnd | exprAnd
//
// exprOr: exprAnd exprOrPrim
// exprOrPrim: OR exprAnd exprOrPrim | epsilon
bool exprOrPrim(Ret* r) {
    Token* start = iTk;
    Ret right;
    if (consume(OR)) {
        if (exprAnd(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for ||");
            *r = (Ret){ {TB_INT,NULL,-1},false,true };

            // if (exprOrPrim(r)) {
            //     return true;
            // }
            // else
            //     tkerr("Invalid expression after && expression");


            // preferred implementation at the next line
            exprOrPrim(r);
            return true;
        }
        else
            tkerr("Invalid expression after || operator");
    }

    iTk = start;
    return true;
}
bool exprOr(Ret* r) {
    Token* start = iTk;
    if (exprAnd(r)) {
        if (exprOrPrim(r)) {
            return true;
        }
        else
            tkerr("Invalid expression after and expression");
    }
    iTk = start;
    return false;
}
bool exprAssign(Ret* r) {
    Ret rDst;
    Token* start = iTk;
    if (exprUnary(&rDst)) {

        if (consume(ASSIGN)) {

            if (exprAssign(r)) {
                if (!rDst.lval)tkerr("the assign destination must be a left-value");
                if (rDst.ct)tkerr("the assign destination cannot be constant");
                if (!canBeScalar(&rDst))tkerr("the assign destination must be scalar");
                if (!canBeScalar(r))tkerr("the assign source must be scalar");
                if (!convTo(&r->type, &rDst.type))tkerr("the assign source cannot be converted to destination");
                r->lval = false;
                r->ct = true;
                return true;
            }
            else
                tkerr("Invalid expression after = operation");
        }
    }
    iTk = start;
    if (exprOr(r)) {
        return true;
    }
    iTk = start;
    return false;
}
bool expr(Ret* r) {
    Token* start = iTk;
    if (exprAssign(r)) {
        return true;
    }
    iTk = start;
    return false;
}

bool stm() {
    Ret rCond, rExpr;
    Token* start = iTk;
    if (stmCompound(true)) {
        return true;
    }
    if (consume(IF)) {
        if (consume(LPAR)) {
            if (expr(&rCond)) {
                if (!canBeScalar(&rCond))tkerr("the if condition must be a scalar value");
                if (consume(RPAR)) {
                    if (stm()) {
                        if (consume(ELSE)) {
                            if (stm()) {}
                        }
                        return true;
                    }
                }
                else
                    tkerr("Missing ) on if loop");
            }
            else
                tkerr("Invalid expression on if loop");
        }
        else
            tkerr("Mission ( on if loop");
    }
    if (consume(WHILE)) {
        if (consume(LPAR)) {
            if (expr(&rExpr)) {
                if (!canBeScalar(&rCond))tkerr("the while condition must be a scalar value");
                if (consume(RPAR)) {
                    if (stm()) {
                        return true;
                    }
                }
                else
                    tkerr("Missing ) on while loop");
            }
            else
                tkerr("Invalid expression on while loop");
        }
        else
            tkerr("Missing ( on while loop");
    }
    if (consume(RETURN)) {
        if (expr(&rExpr)) {
            if (owner->type.tb == TB_VOID)tkerr("a void function cannot return a value");
            if (!canBeScalar(&rExpr))tkerr("the return value must be a scalar value");
            if (!convTo(&rExpr.type, &owner->type))tkerr("cannot convert the return expression type to the function return type");
        }
        else { if (owner->type.tb != TB_VOID)tkerr("a non-void function must return a value"); }

        if (consume(SEMICOLON)) {
            return true;
        }
        else
            tkerr("Missing ; on return statement");
    }

    if (expr(&rExpr)) {}
    if (consume(SEMICOLON)) {
        return true;
    }

    iTk = start;
    return false;
}
bool stmCompound(bool newDomain) {
    Token* start = iTk;
    if (consume(LACC)) {
        if (newDomain)
            pushDomain();
        for (;;) {
            if (varDef()) {}
            else if (stm()) {}
            else
                break;
        }
        if (consume(RACC)) {
            if (newDomain)
                dropDomain();
            return true;
        }
        else
            tkerr("Missing } on stmCompound expression");
    }
    iTk = start;
    return false;
}
bool fnParam() {
    Type t;
    Token* start = iTk;
    if (typeBase(&t)) {
        if (consume(ID)) {
            Token* tkName = consumedTk;
            if (arrayDecl(&t)) { t.n = 0; }
            Symbol* param = findSymbolInDomain(symTable, tkName->text);
            if (param)tkerr("symbol redefinition: %s", tkName->text);
            param = newSymbol(tkName->text, SK_PARAM);
            param->type = t;
            param->owner = owner;
            param->paramIdx = symbolsLen(owner->fn.params);
            //the parameter is added to the current domain and to the function parameters
            addSymbolToDomain(symTable, param);
            addSymbolToList(&owner->fn.params, dupSymbol(param));
            return true;
        }
        else
            tkerr("Missing function parameter name");
    }
    iTk = start;
    return false;
}
bool fnDef() {
    Type t;
    Token* start = iTk;
    if (typeBase(&t)) {
        if (consume(ID)) {
            Token* tkName = consumedTk;
            if (consume(LPAR)) {
                Symbol* fn = findSymbolInDomain(symTable, tkName->text);
                if (fn)tkerr("symbol redefinition: %s", tkName->text);
                fn = newSymbol(tkName->text, SK_FN);
                fn->type = t;
                addSymbolToDomain(symTable, fn);
                owner = fn;
                pushDomain();
                if (fnParam()) {
                    for (;;) {
                        if (consume(COMMA)) {
                            if (fnParam()) {}
                        }
                        else {
                            break;
                        }
                    }
                }
                if (consume(RPAR)) {
                    if (stmCompound(false)) {
                        return true;
                    }
                    else {
                        dropDomain();
                        owner = NULL;
                    }
                }
                else
                    tkerr("Missing ) on function definition");
            }
        }
        else
            tkerr("Missing function name");
    }
    if (consume(VOID)) {
        t.tb = TB_VOID;
        if (consume(ID)) {
            if (consume(LPAR)) {
                if (fnParam()) {
                    for (;;) {
                        if (consume(COMMA)) {
                            if (fnParam()) {}
                        }
                        else {
                            break;
                        }
                    }
                }
                if (consume(RPAR)) {
                    if (stmCompound(false)) {
                        return true;
                    }
                }
                else
                    tkerr("Missing ) on function definition");
            }
            else
                tkerr("Missing ( on function definition");
        }
        tkerr("Mission function name");
    }
    iTk = start;
    return false;
}

bool exprPrimary(Ret* r) {
    Token* start = iTk;
    if (consume(ID)) {
        Token* tkName = consumedTk;
        Symbol* s = findSymbol(tkName->text);
        if (!s)tkerr("undefined id: %s", tkName->text);
        if (consume(LPAR)) {
            if (s->kind != SK_FN)tkerr("only a function can be called");
            Ret rArg;
            Symbol* param = s->fn.params;
            if (expr(&rArg)) {
                if (!param)tkerr("too many arguments in function call");
                if (!convTo(&rArg.type, &param->type))tkerr("in call, cannot convert the argument type to the parameter type");
                param = param->next;
                for (;;) {
                    if (consume(COMMA)) {
                        if (expr(&rArg)) {
                            if (!param)tkerr("too many arguments in function call");
                            if (!convTo(&rArg.type, &param->type))tkerr("in call, cannot convert the argument type to the parameter type");
                            param = param->next;
                        }
                    }
                    else {
                        break;
                    }
                }
            }
            if (consume(RPAR)) {
                if (param)tkerr("too few arguments in function call");
                *r = (Ret){ s->type,false,true };
                // return true; //editflag
            } 
            // else tkerr("Missing ) on function call"); //editflag
            if (s->kind == SK_FN)tkerr("a function can only be called");
            *r = (Ret){ s->type,true,s->type.n >= 0 };

        }
        return true;
    }
    if (consume(INT)) {
        *r=(Ret){{TB_INT,NULL,-1},false,true};
        return true;
    }
    if (consume(DOUBLE)) {
        *r=(Ret){{TB_DOUBLE,NULL,-1},false,true};
        return true;
    }
    if (consume(CHAR)) {
        *r = (Ret){{TB_CHAR,NULL,-1},false,true};
        return true;
    }
    if (consume(STRING)) {
        *r = (Ret){{TB_CHAR,NULL,0},true,true};
        return true;
    }
    if (consume(LPAR)) {
        if (expr(r)) {
            if (consume(RPAR)) {
                return true;
            }
        }
    }
    iTk = start;
    return false;
}
// exprPostfix: exprPostfix LBRACKET expr RBRACKET
//   | exprPostfix DOT ID
//   | exprPrimary
//
// exprPostfix:exprPrimary exprPostfixPrim
// exprPostfixPrim:LBRACKET expr RBRACKET exprPostfixPrim | DOT ID exprPostfixPrim
bool exprPostfixPrim(Ret* r) {
    Token* start = iTk;
    if (consume(LBRACKET)) {
        Ret idx;
        if (expr(&idx)) {
            if (consume(RBRACKET)) {
                if (r->type.n < 0)tkerr("only an array can be indexed");
                Type tInt = { TB_INT,NULL,-1 };
                if (!convTo(&idx.type, &tInt))tkerr("the index is not convertible to int");
                r->type.n = -1;
                r->lval = true;
                r->ct = false;
                if (exprPostfixPrim(r)) {
                    return true;
                }
                else
                    tkerr("Invalid expression after ]");
            }
            else
                tkerr("Missing ] on exprPostfix");
        }
        else
            tkerr("Invalid expression after [ ");
    }
    if (consume(DOT)) {
        if (consume(ID)) {
            Token* tkName = consumedTk;
            if (r->type.tb != TB_STRUCT)tkerr("a field can only be selected from a struct");
            Symbol* s = findSymbolInList(r->type.s->structMembers, tkName->text);
            if (!s)tkerr("the structure %s does not have a field %s", r->type.s->name, tkName->text);
            *r = (Ret){ s->type,true,s->type.n >= 0 };
            if (exprPostfixPrim(r)) {
                return true;
            }
            else
                tkerr("Invalid expression after id");
        }
        else
            tkerr("Invalid id after . operation");
    }

    iTk = start;
    return true;;
}
bool exprPostfix(Ret* r) {
    Token* start = iTk;
    if (exprPrimary(r)) {
        if (exprPostfixPrim(r)) {
            return true;
        }
    }
    iTk = start;
    return false;
}

// unit: ( structDef | fnDef | varDef )* END
bool unit() {
    for (;;) {
        if (structDef()) {}
        else if (fnDef()) {}
        else if (varDef()) {}
        else {
            break;
        }
    }
    if (consume(END)) {
        return true;
    }
    return false;
}

void parse(Token* tokens) {
    iTk = tokens;
    if (!unit())
        tkerr("syntax error"); // nu peste tot syntax error, mai specific: a<b  a b => lipsa operand / expresie invalida ;
}