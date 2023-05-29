#include <stdio.h>
#include <stdlib.h>
#include "lexer.c"
#include "utils.c"
#include "parser.c"
#include "ad.c"
#include "at.h"

int main()
{
    char* inbuf = loadFile("tests/testat.c");
    // puts(inbuf);

    puts(inbuf);
    Token* tokens = tokenize(inbuf);
    showTokens(tokens);
    pushDomain();
    parse(tokens);
    showDomain(symTable, "global");

    dropDomain();
    // showTokens(tokenize(inbuf));
    // parse(tokenize(inbuf));
    return 0;
}
