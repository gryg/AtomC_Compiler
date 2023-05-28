#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>


#include "parser.h"
#include "lexer.h"
#include "ad.h"
#include "utils.h"
#include "at.h"

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

bool exprUnary() {
    Token* start = iTk;
    if (consume(SUB)) {
        if (exprUnary()) {
            return true;
        }
        else
            tkerr("Invalid expression after - character");
    }
    if (consume(NOT)) {
        if (exprUnary()) {
            return true;
        }
        else
            tkerr("Invalid expression after ! character");
    }
    if (exprPostfix()) {
        return true;
    }
    iTk = start;
    return false;
}
bool exprCast() {
    Token* start = iTk;
    if (consume(LPAR)) {
        Type t;
        if (typeBase(&t)) {
            if (arrayDecl(&t)) {}
            if (consume(RPAR)) {
                if (exprCast()) {
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
    else if (exprUnary()) {
        return true;
    }
    iTk = start;
    return false;
}

// exprMul: exprMul ( MUL | DIV ) exprCast | exprCast
//
// exprMul: exprCast exprMulPrim
// exprMulPrim:( MUL | DIV ) exprCast exprMulPrim
bool exprMulPrim() {
    Token* start = iTk;
    if (consume(MUL)) {
        if (exprCast()) {
            if (exprMulPrim()) {
                return true;
            }
            else
                tkerr("Invalid expression after cast operation");
        }
        else
            tkerr("Invalid expression after * operation");
    }
    if (consume(DIV)) {
        if (exprCast()) {
            if (exprMulPrim()) {
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
bool exprMul() {
    Token* start = iTk;
    if (exprCast()) {
        if (exprMulPrim()) {
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
bool exprAddPrim() {
    Token* start = iTk;
    if (consume(ADD)) {
        if (exprMul()) {
            if (exprAddPrim()) {
                return true;
            }
            else
                tkerr("Invalid expression after multiplication expression");
        }
        else
            tkerr("Invalid expression after + operation");
    }
    if (consume(SUB)) {
        if (exprMul()) {
            if (exprAddPrim()) {
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
bool exprAdd() {
    Token* start = iTk;
    if (exprMul()) {
        if (exprAddPrim()) {
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
bool exprRelPrim() {
    Token* start = iTk;
    if (consume(LESS)) {
        if (exprAdd()) {
            if (exprRelPrim()) {
                return true;
            }
            else
                tkerr("Invalid expression after add expression");
        }
        else
            tkerr("Invalid expression after < symbol");
    }
    if (consume(LESSEQ)) {
        if (exprAdd()) {
            if (exprRelPrim()) {
                return true;
            }
            else
                tkerr("Invalid expression after add expression");
        }
        else
            tkerr("Invalid expression after <= symbol");
    }
    if (consume(GREATER)) {
        if (exprAdd()) {
            if (exprRelPrim()) {
                return true;
            }
            else
                tkerr("Invalid expression after add expression");
        }
        else
            tkerr("Invalid expression after > symbol");
    }
    if (consume(GREATEREQ)) {
        if (exprAdd()) {
            if (exprRelPrim()) {
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
bool exprRel() {
    Token* start = iTk;
    if (exprAdd()) {
        if (exprRelPrim()) {
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
bool exprEqPrim() {
    Token* start = iTk;
    if (consume(EQUAL)) {
        if (exprRel()) {
            if (exprEqPrim()) {
                return true;
            }
            else
                tkerr("Invalid expression after rel expression");
        }
        else
            tkerr("Invalid expression after == symbol");
    }
    if (consume(NOTEQ)) {
        if (exprRel()) {
            if (exprEqPrim()) {
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
bool exprEq() {
    Token* start = iTk;
    if (exprRel()) {
        if (exprEqPrim()) {
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

bool exprAndPrim() {
    Token* start = iTk;
    if (consume(AND)) {
        if (exprEq()) {
            if (exprAndPrim()) {
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
bool exprAnd() {
    Token* start = iTk;
    if (exprEq()) {
        if (exprAndPrim()) {
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
bool exprOrPrim() {
    Token* start = iTk;
    if (consume(OR)) {
        if (exprAnd()) {
            if (exprOrPrim()) {
                return true;
            }
            else
                tkerr("Invalid expression after && expression");
        }
        else
            tkerr("Invalid expression after || operator");
    }

    iTk = start;
    return true;
}
bool exprOr() {
    Token* start = iTk;
    if (exprAnd()) {
        if (exprOrPrim()) {
            return true;
        }
        else
            tkerr("Invalid expression after and expression");
    }
    iTk = start;
    return false;
}
bool exprAssign() {
    Token* start = iTk;
    if (exprUnary()) {

        if (consume(ASSIGN)) {

            if (exprAssign()) {

                return true;
            }
            else
                tkerr("Invalid expression after = operation");
        }
    }
    iTk = start;
    if (exprOr()) {

        return true;
    }
    iTk = start;
    return false;
}
bool expr() {
    Token* start = iTk;
    if (exprAssign()) {
        return true;
    }
    iTk = start;
    return false;
}

bool stm() {
    Token* start = iTk;
    if (stmCompound(true)) {
        return true;
    }
    if (consume(IF)) {
        if (consume(LPAR)) {
            if (expr()) {
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
            if (expr()) {
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
        if (expr()) {}
        if (consume(SEMICOLON)) {
            return true;
        }
        else
            tkerr("Missing ; on return statement");
    }

    if (expr()) {}
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

bool exprPrimary() {
    Token* start = iTk;
    if (consume(ID)) {
        if (consume(LPAR)) {
            if (expr()) {
                for (;;) {
                    if (consume(COMMA)) {
                        if (expr()) {}
                    }
                    else {
                        break;
                    }
                }
            }
            if (consume(RPAR)) {}
        }
        return true;
    }
    if (consume(INT)) {
        return true;
    }
    if (consume(DOUBLE)) {
        return true;
    }
    if (consume(CHAR)) {
        return true;
    }
    if (consume(STRING)) {
        return true;
    }
    if (consume(LPAR)) {
        if (expr()) {
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
bool exprPostfixPrim() {
    Token* start = iTk;
    if (consume(LBRACKET)) {
        if (expr()) {
            if (consume(RBRACKET)) {
                if (exprPostfixPrim()) {
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
            if (exprPostfixPrim()) {
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
bool exprPostfix() {
    Token* start = iTk;
    if (exprPrimary()) {
        if (exprPostfixPrim()) {
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