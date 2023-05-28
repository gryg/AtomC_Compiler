
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "lexer.h"
#include "utils.h"


Token *tokens;	// single linked list of tokens
Token *lastTk;		// the last token in list

int line=1;		// the current line in the input file

// adds a token to the end of the tokens list and returns it
// sets its code and line
Token *addTk(int code){
	Token *tk=safeAlloc(sizeof(Token));
	tk->code=code;
	tk->line=line;
	tk->next=NULL;
	if(lastTk){
		lastTk->next=tk;
		}else{
		tokens=tk;
		}
	lastTk=tk;
	return tk;
}

char *extract(const char *begin,const char *end){
    int len=end-begin; // size_t
    char* text=malloc(len+1);//buffer
    /*
    size_t n = end-begin;
    char *buf=safeAlloc(n+1);
    memcpy(buf,begin,n);
    buf[n]='\0';
    return buf;
    */
    if (text == NULL) {
        err("Dynamic memory allocation fail!");
        exit(1);
    }
    strncpy(text, begin, len);
    text[len] = '\0';
    return text;
	}

Token *tokenize(const char *pch) {
    const char *start;
    Token *tk;
    for (;;) {
        switch (*pch) {
            case ' ':
            case '\t':
                pch++;
                break;
            case '\r': // handles different kinds of newlines (Windows: \r\n, Linux: \n, MacOS, OS X: \r or \n)
                if (pch[1] == '\n') pch++;
                // fallthrough to \n
            case '\n':
                line++;
                pch++;
                break;
            case '\0':
                addTk(END);
                return tokens;
            case ',':
                addTk(COMMA);
                pch++;
                break;
            case ';':
                addTk(SEMICOLON);
                pch++;
                break;
            case '+':
                addTk(ADD);
                pch++;
                break;
            case '(':
                addTk(LPAR);
                pch++;
                break;
            case ')':
                addTk(RPAR);
                pch++;
                break;
            case '{':
                addTk(LACC);
                pch++;
                break;
            case '}':
                addTk(RACC);
                pch++;
                break;
            case '[':
                addTk(LBRACKET);
                pch++;
                break;
            case ']':
                addTk(RBRACKET);
                pch++;
                break;
            case '-':
                addTk(SUB);
                pch++;
                break;
            case '*':
                addTk(MUL);
                pch++;
                break;
            case '/':
                if(pch[1] == '/'){
                    while (*pch !='\n') pch++;
                }
                else if(pch[1] == '*'){ 
                    while(strncmp(pch,"*/", 2) != 0){
                    if (*pch == '\n') line++;
                    pch++;
                }
                    pch+=2;
                }
                else{
                addTk(DIV);
                pch++;
            }
                break;
            case '.':
                addTk(DOT);
                pch++;
                break;
            case '&':
                if (pch[1] == '&') {
                    addTk(AND);
                    pch += 2;
                } else {
                    err("invalid operator: &");
                }
                break;
            case '|':
                if (pch[1] == '|') {
                    addTk(OR);
                    pch += 2;
                } else {
                    err("invalid operator: |");
                }
                break;
            case '!':
                if (pch[1] == '=') {
                    addTk(NOTEQ);
                    pch += 2;
                } else {
                    addTk(NOT);
                    pch++;
                }
                break;
            case '=':
                if (pch[1] == '=') {
                    addTk(EQUAL);
                    pch += 2;
                } else {
                    addTk(ASSIGN);
                    pch++;
                }
                break;
            case '<':
                if (pch[1] == '=') {
                    addTk(LESSEQ);
                    pch += 2;
                } else {
                    addTk(LESS);
                    pch++;
                }
                break;
            case '>':
                if (pch[1] == '=') {
                    addTk(GREATEREQ);
                    pch += 2;
                } else {
                    addTk(GREATER);
                    pch++;
                }
                break;
            default:
                if (isalpha(*pch) || *pch == '_') {
                    for (start = pch++; isalnum(*pch) || *pch == '_'; pch++) {}
                    char *text = extract(start, pch);
                    if (strcmp(text, "int") == 0)
                        addTk(TYPE_INT);
                    else if (strcmp(text, "double") == 0)
                        addTk(TYPE_DOUBLE);
                    else if (strcmp(text, "char") == 0)
                        addTk(TYPE_CHAR);
                    else if (strcmp(text, "else") == 0)
                        addTk(ELSE);
                    else if (strcmp(text, "if") == 0)
                        addTk(IF);
                    else if (strcmp(text, "return") == 0)
                        addTk(RETURN);
                    else if (strcmp(text, "struct") == 0)
                        addTk(STRUCT);
                    else if (strcmp(text, "void") == 0)
                        addTk(VOID);
                    else if (strcmp(text, "while") == 0)
                        addTk(WHILE);
                    else {
                        tk = addTk(ID);
                        tk->text = text;
                    }


                }
                else if (isdigit(*pch) || (*pch == '.' && isdigit(*(pch+1)))) {
                    int is_double = 0;
                    const char *start = pch;
                    while (isdigit(*pch)) pch++;
                    if (*pch == '.') {
                        is_double = 1;
                        pch++;
                        while (isdigit(*pch)) pch++;
                    }
                    if (*pch == 'e' || *pch == 'E') {
                        is_double = 1;
                        pch++;
                        if (*pch == '+' || *pch == '-') pch++;
                        while (isdigit(*pch)) pch++;
                    }
                    char *text = extract(start, pch);//extract e tehnic slicing in 
                    if (is_double) {
                        tk = addTk(DOUBLE);
                        tk->d = atof(text);
                        } else {
                            tk = addTk(INT);
                            tk->i = atoi(text);
                        }
                        break;
                    }
                    else if(*pch=='\''){
                    if(pch[1]!='\''&&pch[2]=='\''){
                        tk=addTk(CHAR);
                        tk->c=pch[1];
                        pch+=3;
                    }else err("invalid char: %c (%d)",*pch,*pch);
                }else if(*pch=='"'){
                    for(start=pch++;*pch!='\"';pch++){}
                    char *text=extract(start+1,pch);
                    tk=addTk(STRING);
                    tk->text=text;
                    pch++;
                }

                    else
                    err("invalid char: %c (%d)", *pch, *pch);
        }
    }
}
// void showTokens(const Token *tokens){
// 	for(const Token *tk=tokens;tk;tk=tk->next){
// 		printf("%d\n",tk->code);
// 		}
// 	}




void showTokens(const Token *tokens){
    for(const Token *tk=tokens;tk;tk=tk->next){
        switch(tk->code){
        case 0: printf("%d       %s%s\n",tk->line,"ID:",tk->text);break;
        case 1: printf("%d       %s\n",tk->line,"TYPE_CHAR",tk->text);break;
        case 2: printf("%d       %s\n",tk->line,"TYPE_INT",tk->text);break;
        case 3: printf("%d       %s\n",tk->line,"TYPE_DOUBLE",tk->text);break;
        case 4: printf("%d       %s\n",tk->line,"ELSE",tk->text);break;
        case 5: printf("%d       %s\n",tk->line,"IF",tk->text);break;
        case 6: printf("%d       %s\n",tk->line,"RETURN",tk->text);break;
        case 7: printf("%d       %s\n",tk->line,"STRUCT",tk->text);break;
        case 8: printf("%d       %s\n",tk->line,"VOID",tk->i);break;
        case 9: printf("%d       %s\n",tk->line,"WHILE",tk->text);break;
        case 10: printf("%d       %s%d\n",tk->line,"INT:",tk->text);break;
        case 11: printf("%d       %s%lf\n",tk->line,"DOUBLE:",tk->d);break;
        case 12: printf("%d       %s%c\n",tk->line,"CHAR:",tk->c);break;
        case 13: printf("%d       %s%s\n",tk->line,"STRING:",tk->text);break;
        case 14: printf("%d       %s\n",tk->line,"COMMA",tk->text);break;
        case 15: printf("%d       %s\n",tk->line,"END",tk->text);break;
        case 16: printf("%d       %s\n",tk->line,"SEMICOLON",tk->text);break;
        case 17: printf("%d       %s\n",tk->line,"LPAR",tk->text);break;
        case 18: printf("%d       %s\n",tk->line,"RPAR",tk->text);break;
        case 19: printf("%d       %s\n",tk->line,"LBRACKET",tk->text);break;
        case 20: printf("%d       %s\n",tk->line,"RBRACKET",tk->text);break;
        case 21: printf("%d       %s\n",tk->line,"LACC",tk->text);break;
        case 22: printf("%d       %s\n",tk->line,"RACC",tk->text);break;
        case 23: printf("%d       %s\n",tk->line,"ASSIGN",tk->text);break;
        case 24: printf("%d       %s\n",tk->line,"EQUAL",tk->text);break;
        case 25: printf("%d       %s\n",tk->line,"ADD",tk->text);break;
        case 26: printf("%d       %s\n",tk->line,"SUB",tk->text);break;
        case 27: printf("%d       %s\n",tk->line,"MUL",tk->text);break;
        case 28: printf("%d       %s\n",tk->line,"DIV",tk->text);break;
        case 29: printf("%d       %s\n",tk->line,"DOT",tk->text);break;
        case 30: printf("%d       %s\n",tk->line,"AND",tk->text);break;
        case 31: printf("%d       %s\n",tk->line,"OR",tk->text);break;
        case 32: printf("%d       %s\n",tk->line,"NOT",tk->text);break;
        case 33: printf("%d       %s\n",tk->line,"NOTEQ",tk->text);break;
        case 34: printf("%d       %s\n",tk->line,"LESS",tk->text);break;
        case 35: printf("%d       %s\n",tk->line,"LESSEQ",tk->text);break;
        case 36: printf("%d       %s\n",tk->line,"GREATER",tk->text);break;
        case 37: printf("%d       %s\n",tk->line,"GREATEREQ",tk->text);break;
        }
    }
}