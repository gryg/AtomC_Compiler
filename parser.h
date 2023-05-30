#pragma once

#include "lexer.h"
#include "ad.h"
#include "utils.h"
#include "at.h"
#include "gc.h"
#include "vm.h"

Token* iTk;
Token* consumedTk;
Symbol* owner;

void parse(Token *tokens);
void tkerr(const char* fmt, ...);


bool consume(int code);
bool unit();
bool structDef();
bool varDef();
bool typeBase();
bool arrayDecl();
bool fnDef();
bool fnParam();
bool stm();
bool stmCompound(bool newDomain);
bool expr();
bool exprAssign();
bool exprOr();
bool exprAnd();
bool exprEq();
bool exprRel();
bool exprAdd();
bool exprMul();
bool exprCast();
bool exprUnary();
bool exprPostfix();
bool exprPrimary();