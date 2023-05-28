#include <stdio.h>
#include <stdlib.h>
#include "lexer.c"
#include "utils.c"
#include "parser.c"

int main()
{
    char *inbuf = loadFile("tests/testparser.c");
    // puts(inbuf);

    puts(inbuf);
    Token *tokens = tokenize(inbuf);
    showTokens(tokens);
    parse(tokens);
    // showTokens(tokenize(inbuf));
    // parse(tokenize(inbuf));
    return 0;
}
