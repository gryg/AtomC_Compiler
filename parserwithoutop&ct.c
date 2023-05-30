#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"
// #include "gc.c"

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
    else tkerr("Syntax error!");
    return false;
}

bool structDef() {
    Token* start = iTk;
    if (consume(STRUCT)) {
        if (consume(ID)) {
            Token* tkName = consumedTk;
            if (consume(LACC)) {
                Symbol* s = findSymbolInDomain(symTable, tkName->text);
                if (s) tkerr("symbol redefinition: %s", tkName->text);
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
                        dropDomain();
                        return true;
                    }
                    else tkerr("Missing ; after struct definition!");
                }
                else tkerr("Missing '}' in struct definition!");
            }
        }
        else tkerr("Struct does not have an identifier!");
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
                if (t.n == 0)
                    tkerr("A vector variable must have a specified dimension");
            }
            if (consume(SEMICOLON)) {
                Symbol*
                    var = findSymbolInDomain(symTable, tkName->text);
                if (var) tkerr("symbol redefinition: %s", tkName->text);
                var = newSymbol(tkName->text, SK_VAR);
                var->type = t;
                var->owner = owner;
                addSymbolToDomain(symTable,
                    var);
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
            else tkerr("Missing ; after the variable declaration!");
        }
        else tkerr("Missing variable name!");
    }
    iTk = start;
    return false;
}

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
            if (!t->s)
                tkerr("Structure undefined: %s", tkName->text);
            return true;
        }
        else tkerr("Structure does not have a name!");
    }
    iTk = start;
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
        else tkerr("Missing ] or invalid expression inside [...]\n");
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
                if (fn) tkerr("Symbol redefinition: %s", tkName->text);
                fn = newSymbol(tkName->text, SK_FN);
                fn->type = t;
                addSymbolToDomain(symTable, fn);
                owner = fn;
                pushDomain();
                if (fnParam()) {
                    while (consume(COMMA)) {
                        if (fnParam()) {}
                        else tkerr("Missing parameter after , in function's header!");
                    }
                }
                if (consume(RPAR)) {
                    addInstr(&fn->fn.instr, OP_ENTER);
                    if (stmCompound(false)) {
                        fn->fn.instr->arg.i = symbolsLen(fn->fn.locals);
                        if (fn->type.tb == TB_VOID)
                            addInstrWithInt(&fn->fn.instr, OP_RET_VOID, symbolsLen(fn->fn.params));
                        dropDomain();
                        owner = NULL;
                        return true;
                    }
                    else tkerr("Expected body for function declared!");
                }
                else tkerr("Missing ) in function header!");
            }
        }
        else tkerr("Expected function identifier!");
    }

    if (consume(VOID)) {
        t.tb = TB_VOID;
        if (consume(ID)) {
            Token* tkName = consumedTk;
            if (consume(LPAR)) {
                Symbol* fn = findSymbolInDomain(symTable, tkName->text);
                if (fn) tkerr("symbol redefinition: %s", tkName->text);
                fn = newSymbol(tkName->text, SK_FN);
                fn->type = t;
                addSymbolToDomain(symTable, fn);
                owner = fn;
                pushDomain();
                if (fnParam()) {
                    for (;;) {
                        if (consume(COMMA)) {
                            if (fnParam()) {}
                            else tkerr("Missing parameter after ,");
                        }
                        else break;
                    }
                }
                if (consume(RPAR)) {
                    if (stmCompound(false)) {
                        dropDomain();
                        owner = NULL;
                        return true;
                    }
                    else tkerr("Expected body for function declared!");
                }
                else tkerr("Missing ) in the function header!");
            }
        }
        else tkerr("Missing function name!");
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
            if (arrayDecl(&t)) {
                t.n = 0;
            }
            Symbol* param = findSymbolInDomain(symTable, tkName->text);
            if (param) tkerr("symbol redefinition: %s", tkName->text);
            param = newSymbol(tkName->text, SK_PARAM);
            param->type = t;
            param->owner = owner;
            param->paramIdx = symbolsLen(owner->fn.params);
            addSymbolToDomain(symTable, param);
            addSymbolToList(&owner->fn.params, dupSymbol(param));
            return true;
        }
        else tkerr("Expected function parameter name(s)!");
    }
    iTk = start;
    return false;
}

bool stm() {
    Token* start = iTk;
    Ret rCond, rExpr;
    if (stmCompound(true)) {
        return true;
    }
    if (consume(IF)) {
        if (consume(LPAR)) {
            if (expr(&rCond)) {
                {
                    if (!canBeScalar(&rCond)) tkerr("the if condition must be a scalar value");
                }
                if (consume(RPAR)) {
                    addRVal(&owner->fn.instr, rCond.lval, &rCond.type);
                    Type intType = { TB_INT,NULL,-1 };
                    insertConvIfNeeded(lastInstr(owner->fn.instr), &rCond.type, &intType);
                    Instr* ifJF = addInstr(&owner->fn.instr, OP_JF);
                    if (stm()) {
                        if (consume(ELSE)) {
                            Instr* ifJMP = addInstr(&owner->fn.instr, OP_JMP);
                            ifJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
                            if (stm()) { ifJMP->arg.instr = addInstr(&owner->fn.instr, OP_NOP); }
                            else {
                                ifJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP); //might break here
                                tkerr("Missing statement after ELSE!");
                            }
                        }
                        return true;
                    }
                    else tkerr("Missing statement after IF!");
                }
                else tkerr("Missing ) after expression!");
            }
            else tkerr("Missing expression!");
        }
        else tkerr("Missing ( after IF!");
    }
    if (consume(WHILE)) {
        Instr* beforeWhileCond = lastInstr(owner->fn.instr);
        if (consume(LPAR)) {
            if (expr(&rCond)) {
                {
                    if (!canBeScalar(&rCond)) tkerr("the while condition must be a scalar value");
                }
                if (consume(RPAR)) {
                    addRVal(&owner->fn.instr, rCond.lval, &rCond.type);
                    Type intType = { TB_INT,NULL,-1 };
                    insertConvIfNeeded(lastInstr(owner->fn.instr), &rCond.type, &intType);
                    Instr* whileJF = addInstr(&owner->fn.instr, OP_JF);
                    if (stm()) {
                        addInstr(&owner->fn.instr, OP_JMP)->arg.instr = beforeWhileCond->next;
                        whileJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
                        return true;
                    }
                    else tkerr("Expected statement after WHILE!");
                }
                else tkerr("Missing ) after expression!");
            }
        }
        else tkerr("Expected expression between ()!");
    }
    if (consume(RETURN)) {
        if (expr(&rExpr)) {
            addRVal(&owner->fn.instr, rExpr.lval, &rExpr.type);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &rExpr.type, &owner->type);
            addInstrWithInt(&owner->fn.instr, OP_RET, symbolsLen(owner->fn.params));
            if (owner->type.tb == TB_VOID) tkerr("a void function cannot return a value");
            if (!canBeScalar(&rExpr)) tkerr("the return value must be a scalar value");
            if (!convTo(&rExpr.type, &owner->type)) tkerr("cannot convert the return expression type to the function return type");
        }
        else {
            addInstr(&owner->fn.instr, OP_RET_VOID); //might break here
            if (owner->type.tb != TB_VOID) tkerr("a non-void function must return a value");
        }
        if (consume(SEMICOLON)) {
            return true;
        }
        else tkerr("Expected ;");
    }
    if (expr(&rExpr)) {
        if (rExpr.type.tb != TB_VOID)addInstr(&owner->fn.instr, OP_DROP);
        if (consume(SEMICOLON)) {
            return true;
        }
        else tkerr("Missing ; after expression!");
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
            else {
                break;
            }
        }
        if (consume(RACC)) {
            if (newDomain)
                dropDomain();
            return true;
        }
        else tkerr("Missing } after function block!");
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

bool exprAssign(Ret* r) {
    Token* start = iTk;
    Ret rDst;
    if (exprUnary(&rDst)) {
        if (consume(ASSIGN)) {
            if (exprAssign(r)) {
                addRVal(&owner->fn.instr, r->lval, &r->type);
                insertConvIfNeeded(lastInstr(owner->fn.instr), &r->type, &rDst.type);
                switch (rDst.type.tb) {
                case TB_INT:addInstr(&owner->fn.instr, OP_STORE_I);break;
                case TB_DOUBLE:addInstr(&owner->fn.instr, OP_STORE_F);break;
                }
                if (!rDst.lval) tkerr("the assign destination must be a left-value");
                if (rDst.ct) tkerr("the assign destination cannot be constant");
                if (!canBeScalar(&rDst)) tkerr("the assign destination must be scalar");
                if (!canBeScalar(r)) tkerr("the assign source must be scalar");
                if (!convTo(&r->type, &rDst.type)) tkerr("the assign source cannot be converted todestination");
                r->lval = false;
                r->ct = true;
                return true;
            }
            else tkerr("Expected expression after = !");
        }
    }
    iTk = start;
    if (exprOr(r)) {
        return true;
    }
    iTk = start;
    return false;
}

bool exprOrPrim(Ret* r) {
    Token* start = iTk;
    if (consume(OR)) {
        Ret right;
        if (exprAnd(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for ||");
            *r = (Ret){
                {
                    TB_INT,
                    NULL,
                    -1
                }, false, true
            };
            if (exprOrPrim(r));
            return true;
        }
        else tkerr("invalid expression after ||");
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
    }
    iTk = start;
    return false;
}

bool exprAndPrim(Ret* r) {
    Token* start = iTk;
    if (consume(AND)) {
        Ret right;
        if (exprEq(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for &&");
            *r = (Ret){
                {
                    TB_INT,
                    NULL,
                    -1
                }, false, true
            };
            if (exprAndPrim(r)) {
                return true;
            }
        }
        else tkerr("Invalid expression after AND");
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
    }
    iTk = start;
    return false;
}

bool exprEqPrim(Ret* r) {
    Token* start = iTk;
    if (consume(EQUAL)) {
        Ret right;
        if (exprRel(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for == or !=");
            *r = (Ret){
                {
                    TB_INT,
                    NULL,
                    -1
                }, false, true
            };
            if (exprEqPrim(r)) {
                return true;
            }
        }
        else tkerr("Invalid expression after equal");
    }
    if (consume(NOTEQ)) {
        Ret right;
        if (exprRel(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for == or !=");
            *r = (Ret){
                {
                    TB_INT,
                    NULL,
                    -1
                }, false, true
            };
            if (exprEqPrim(r)) {
                return true;
            }
        }
        else tkerr("Invalid expression after not equal");
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
    }
    iTk = start;
    return false;
}

bool exprRelPrim(Ret* r) {
    Token* start = iTk;
    Token* op;
    if (consume(LESS[op])) {
        Ret right;
        Instr* lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);
        if (exprAdd(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for <, <=, >,>=");
            addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code) {
            case LESS:
                switch (tDst.tb) {
                case TB_INT:addInstr(&owner->fn.instr, OP_LESS_I);break;
                case TB_DOUBLE:addInstr(&owner->fn.instr, OP_LESS_F);break;
                }
                break;
            }
            *r = (Ret){
                {
                    TB_INT,
                    NULL,
                    -1
                }, false, true
            };
            if (exprRelPrim(r)) {
                return true;
            }
        }
        else tkerr("Invalid expression after less symbol");
    }
    if (consume(LESSEQ[op])) {
        Ret right;
        Instr* lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);
        if (exprAdd(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for <, <=, >, >=");
            addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code) {
            case LESS:
                switch (tDst.tb) {
                case TB_INT:addInstr(&owner->fn.instr, OP_LESS_I);break;
                case TB_DOUBLE:addInstr(&owner->fn.instr, OP_LESS_F);break;
                }
                break;
            }
            *r = (Ret){
                {
                    TB_INT,
                    NULL,
                    -1
                }, false, true
            };
            if (exprRelPrim(r)) {
                return true;
            }
        }
        else tkerr("Invalid expression after less than equal symbol");
    }
    if (consume(GREATER[op])) {
        Ret right;
        Instr* lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);
        if (exprAdd(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for <, <=, >, >=");
            addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code) {
            case LESS:
                switch (tDst.tb) {
                case TB_INT:addInstr(&owner->fn.instr, OP_LESS_I);break;
                case TB_DOUBLE:addInstr(&owner->fn.instr, OP_LESS_F);break;
                }
                break;
            }
            *r = (Ret){
                {
                    TB_INT,
                    NULL,
                    -1
                }, false, true
            };
            if (exprRelPrim(r)) {
                return true;
            }
        }
        else tkerr("Invalid expression after greater symbol");
    }
    if (consume(GREATEREQ[op])) {
        Ret right;
        Instr* lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);
        if (exprAdd(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for <, <=, >, >=");
            addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code) {
            case LESS:
                switch (tDst.tb) {
                case TB_INT:addInstr(&owner->fn.instr, OP_LESS_I);break;
                case TB_DOUBLE:addInstr(&owner->fn.instr, OP_LESS_F);break;
                }
                break;
            }
            *r = (Ret){
                {
                    TB_INT,
                    NULL,
                    -1
                }, false, true
            };
            if (exprRelPrim(r)) {
                return true;
            }
        }
        else tkerr("Invalid expression after greater equal symbol");
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
    }
    iTk = start;
    return false;
}

bool exprAddPrim(Ret* r) {
    Token* start = iTk;
    Token* op;
    if (consume(ADD[op])) {
        Instr* lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);
        Ret right;
        if (exprMul(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for + or -");
            addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code) {
            case ADD:
                switch (tDst.tb) {
                case TB_INT:addInstr(&owner->fn.instr, OP_ADD_I);break;
                case TB_DOUBLE:addInstr(&owner->fn.instr, OP_ADD_F);break;
                }
                break;
            case SUB:
                switch (tDst.tb) {
                case TB_INT:addInstr(&owner->fn.instr, OP_SUB_I);break;
                case TB_DOUBLE:addInstr(&owner->fn.instr, OP_SUB_F);break;
                }
                break;
            }
            *r = (Ret){
                tDst,
                false,
                true
            };
            if (exprAddPrim(r)) {
                return true;
            }
        }
        else tkerr("Invalid expression after + ");
    }
    if (consume(SUB[op])) {
        Instr* lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);
        Ret right;
        if (exprMul(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for + or -");
            addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code) {
            case ADD:
                switch (tDst.tb) {
                case TB_INT:addInstr(&owner->fn.instr, OP_ADD_I);break;
                case TB_DOUBLE:addInstr(&owner->fn.instr, OP_ADD_F);break;
                }
                break;
            case SUB:
                switch (tDst.tb) {
                case TB_INT:addInstr(&owner->fn.instr, OP_SUB_I);break;
                case TB_DOUBLE:addInstr(&owner->fn.instr, OP_SUB_F);break;
                }
                break;
            }
            *r = (Ret){
                tDst,
                false,
                true
            };
            if (exprAddPrim(r)) {
                return true;
            }
        }
        else tkerr("Invalid expression after - symbol");
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
    }
    iTk = start;
    return false;
}

bool exprMulPrim(Ret* r) {
    Token* op;
    Token* start = iTk;
    if (consume(MUL[op])) {
        Instr* lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);
        Ret right;
        if (exprCast(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for * or /");
            addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code) {
            case MUL:
                switch (tDst.tb) {
                case TB_INT:addInstr(&owner->fn.instr, OP_MUL_I);break;
                case TB_DOUBLE:addInstr(&owner->fn.instr, OP_MUL_F);break;
                }
                break;
            case DIV:
                switch (tDst.tb) {
                case TB_INT:addInstr(&owner->fn.instr, OP_DIV_I);break;
                case TB_DOUBLE:addInstr(&owner->fn.instr, OP_DIV_F);break;
                }
                break;
            }
            *r = (Ret){
                tDst,
                false,
                true
            };
            if (exprMulPrim(r)) {
                return true;
            }
        }
        else tkerr("Invalid expression after * symbol");
    }
    if (consume(DIV[op])) {
        Ret right;
        Instr* lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);
        if (exprCast(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) tkerr("invalid operand type for * or /");
            addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code) {
            case MUL:
                switch (tDst.tb) {
                case TB_INT:addInstr(&owner->fn.instr, OP_MUL_I);break;
                case TB_DOUBLE:addInstr(&owner->fn.instr, OP_MUL_F);break;
                }
                break;
            case DIV:
                switch (tDst.tb) {
                case TB_INT:addInstr(&owner->fn.instr, OP_DIV_I);break;
                case TB_DOUBLE:addInstr(&owner->fn.instr, OP_DIV_F);break;
                }
                break;
            }
            *r = (Ret){
                tDst,
                false,
                true
            };
            if (exprMulPrim(r)) {
                return true;
            }
        }
        else tkerr("Invalid expression after div");
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
            if (arrayDecl(&t))
                if (consume(RPAR)) {
                    if (exprCast(&op)) {
                        if (t.tb == TB_STRUCT) tkerr("cannot convert to a struct type");
                        if (op.type.tb == TB_STRUCT) tkerr("cannot convert a struct");
                        if (op.type.n >= 0 && t.n < 0) tkerr("an array can be converted only to another array");
                        if (op.type.n < 0 && t.n >= 0) tkerr("a scalar can be converted only to another scalar");
                        *r = (Ret){
                            t,
                            false,
                            true
                        };
                        return true;
                    }
                }
                else tkerr("Missing ) ");
        }
    }
    if (exprUnary(r)) {
        return true;
    }
    iTk = start;
    return false;
}

bool exprUnary(Ret* r) {
    Token* start = iTk;
    if (consume(SUB)) {
        if (exprUnary(r)) {
            if (!canBeScalar(r)) tkerr("unary - or ! must have a scalar operand");
            r->lval = false;
            r->ct = true;
            return true;
        }
    }
    if (consume(NOT)) {
        if (exprUnary(r)) {
            if (!canBeScalar(r)) tkerr("unary - or ! must have a scalar operand");
            r->lval = false;
            r->ct = true;
            return true;
        }
    }
    if (exprPostfix(r)) {
        return true;
    }
    iTk = start;
    return false;
}

bool exprPostfixPrim(Ret* r) {
    Token* start = iTk;
    if (consume(LBRACKET)) {
        Ret idx;
        if (expr(&idx)) {
            if (consume(RBRACKET)) {
                if (r->type.n < 0) tkerr("only an array can be indexed");
                Type tInt = {
                    TB_INT,
                    NULL,
                    -1
                };
                if (!convTo(&idx.type, &tInt)) tkerr("the index is not convertible to int");
                r->type.n = -1;
                r->lval = true;
                r->ct = false;
                if (exprPostfixPrim(r)) {
                    return true;
                }
            }
            else tkerr("Missing ]");
        }
        else tkerr("Expected expression after [ ");
    }
    if (consume(DOT)) {
        if (consume(ID)) {
            Token* tkName = consumedTk;
            if (r->type.tb != TB_STRUCT) tkerr("A field can only be selected from a STRUCT!");
            Symbol* s = findSymbolInList(r->type.s->structMembers, tkName->text);
            if (!s) tkerr("The structure %s does not have a field %s !", r->type.s->name, tkName->text);
            *r = (Ret){
                s->type, true, s->type.n >= 0
            };
            if (exprPostfixPrim(r)) {
                return true;
            }
        }
        else tkerr("Expected identifier");
    }
    iTk = start;
    return true;
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

bool exprPrimary(Ret* r) {
    if (consume(ID)) {
        Token* tkName = consumedTk;
        Symbol* s = findSymbol(tkName->text);
        if (!s)
            tkerr("Undefined ID: %s !", tkName->text);
        if (consume(LPAR)) {
            if (s->kind != SK_FN) tkerr("only a function can be called");
            Ret rArg;
            Symbol* param = s->fn.params;
            if (expr(&rArg)) {
                if (!param) tkerr("too many arguments in function call");
                if (!convTo(&rArg.type, &param->type)) tkerr("in call, cannot convert the argument type to the parameter type");
                addRVal(&owner->fn.instr, rArg.lval, &rArg.type);
                insertConvIfNeeded(lastInstr(owner->fn.instr), &rArg.type, &param->type);
                param = param->next;
                for (;;) {
                    if (consume(COMMA)) {
                        if (expr(&rArg)) {
                            if (!param) tkerr("too many arguments in function call");
                            if (!convTo(&rArg.type, &param->type)) tkerr("in call, cannot convert the argument type to the parameter type");
                            addRVal(&owner->fn.instr, rArg.lval, &rArg.type);
                            insertConvIfNeeded(lastInstr(owner->fn.instr), &rArg.type, &param->type);
                            param = param->next;
                        }
                        else tkerr("Expected expression after ,");
                    }
                    else break;
                }
            }
            if (consume(RPAR)) {
                if (s->fn.extFnPtr) {
                    addInstr(&owner->fn.instr, OP_CALL_EXT)->arg.extFnPtr = s->fn.extFnPtr;
                }
                else {
                    addInstr(&owner->fn.instr, OP_CALL)->arg.instr = s->fn.instr;
                }
                if (param) tkerr("too few arguments in function call");
                *r = (Ret){
                    s->type, false, true
                };
                return true;
            }
            else {
                if (s->kind == SK_VAR) {
                    if (s->owner == NULL) { // global variables
                        addInstr(&owner->fn.instr, OP_ADDR)->arg.p = s->varMem;
                    }
                    else { // local variables
                        switch (s->type.tb) {
                        case TB_INT:addInstrWithInt(&owner->fn.instr, OP_FPADDR_I, s->varIdx + 1);break;
                        case TB_DOUBLE:addInstrWithInt(&owner->fn.instr, OP_FPADDR_F, s->varIdx + 1);break;
                        }
                    }
                }
                if (s->kind == SK_PARAM) {
                    switch (s->type.tb) {
                    case TB_INT:
                        addInstrWithInt(&owner->fn.instr, OP_FPADDR_I, s->paramIdx - symbolsLen(s->owner->fn.params) -
                            1); break;
                    case TB_DOUBLE:
                        addInstrWithInt(&owner->fn.instr, OP_FPADDR_F, s->paramIdx - symbolsLen(s->owner->fn.params) -
                            1); break;
                    }
                }
                tkerr("Missing ) ");
            }
        }
        if (s->kind == SK_FN) tkerr("a function can only be called");
        *r = (Ret){
            s->type, true, s->type.n >= 0
        };
        return true;
    }
    if (consume(INT[&ct])) {
        addInstrWithInt(&owner->fn.instr, OP_PUSH_I, ct->i);
        *r = (Ret){
            {
                TB_INT,
                NULL,
                -1
            }, false, true
        };
        return true;
    }
    if (consume(DOUBLE[&ct])) {
        addInstrWithDouble(&owner->fn.instr, OP_PUSH_F, ct->d);
        *r = (Ret){
            {
                TB_DOUBLE,
                NULL,
                -1
            }, false, true
        };
        return true;
    }
    if (consume(CHAR[&ct])) {
        *r = (Ret){
            {
                TB_CHAR,
                NULL,
                -1
            }, false, true
        };
        return true;
    }
    if (consume(STRING[&ct])) {
        *r = (Ret){
            {
                TB_CHAR,
                NULL,
                0
            }, false, true
        };
        return true;
    }
    if (consume(LPAR)) {
        if (expr(r)) {
            if (consume(RPAR)) {
                return true;
            }
            else tkerr("Missing ) ");
        }
    }

}

void parse(Token* tokens) {
    iTk = tokens;
    if (!unit()) {
        tkerr("Syntax error!");
    }
}