#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
#include "parser.h"

Token *iTk;        // the iterator in the tokens list
Token *consumedTk; // the last consumed token
bool expr();
bool exprPostfix();
bool stmCompound();
bool stm();
void tkerr(const char *fmt, ...)
{
    fprintf(stderr, "error in line %d: ", iTk->line);
    va_list va;
    va_start(va, fmt);
    vfprintf(stderr, fmt, va);
    va_end(va);
    fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
}



bool consume(int code)
{
    if (iTk->code == code)
    {
        consumedTk = iTk;
        iTk = iTk->next;
        return true;
    }
    return false;
}

bool arrayDecl()
{
    Token *start = iTk;
    if (consume(LBRACKET))
    {
        if (consume(INT))
        {
        }
        if (consume(RBRACKET))
        {
            return true;
        }
        else
            tkerr("Missing ] after array declaration\n");
    }
    iTk = start;
    return false;
}
// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase()
{

    if (consume(TYPE_INT))
    {
        return true;
    }
    if (consume(TYPE_DOUBLE))
    {
        return true;
    }
    if (consume(TYPE_CHAR))
    {
        return true;
    }
    if (consume(STRUCT))
    {
        if (consume(ID))
        {
            return true;
        }
    }

    return false;
}

bool varDef()
{
    Token *start = iTk;
    if (typeBase())
    {
        if (consume(ID))
        {
            if (arrayDecl())
            {
            }
            if (consume(SEMICOLON))
            {
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
bool structDef()
{
    Token *start = iTk;
    if (consume(STRUCT))
    {
        if (consume(ID))
        {
            if (consume(LACC))
            {
                for (;;)
                {
                    if (varDef())
                    {
                    }
                    else
                    {
                        break;
                    }
                }
                if (consume(RACC))
                {
                    if (consume(SEMICOLON))
                    {
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

bool exprUnary()
{
    Token *start = iTk;
    if (consume(SUB))
    {
        if (exprUnary())
        {
            return true;
        }
        else
            tkerr("Invalid expression after - character");
    }
    if (consume(NOT))
    {
        if (exprUnary())
        {
            return true;
        }
        else
            tkerr("Invalid expression after ! character");
    }
    if (exprPostfix())
    {
        return true;
    }
    iTk = start;
    return false;
}
bool exprCast()
{
    Token *start = iTk;
    if (consume(LPAR))
    {
        if (typeBase())
        {
            if (arrayDecl())
            {
            }
            if (consume(RPAR))
            {
                if (exprCast())
                {
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
    else if (exprUnary())
    {
        return true;
    }
    iTk = start;
    return false;
}

// exprMul: exprMul ( MUL | DIV ) exprCast | exprCast
//
// exprMul: exprCast exprMulPrim
// exprMulPrim:( MUL | DIV ) exprCast exprMulPrim
bool exprMulPrim()
{
    Token *start = iTk;
    if (consume(MUL))
    {
        if (exprCast())
        {
            if (exprMulPrim())
            {
                return true;
            }
            else
                tkerr("Invalid expression after cast operation");
        }
        else
            tkerr("Invalid expression after * operation");
    }
    if (consume(DIV))
    {
        if (exprCast())
        {
            if (exprMulPrim())
            {
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
bool exprMul()
{
    Token *start = iTk;
    if (exprCast())
    {
        if (exprMulPrim())
        {
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
bool exprAddPrim()
{
    Token *start = iTk;
    if (consume(ADD))
    {
        if (exprMul())
        {
            if (exprAddPrim())
            {
                return true;
            }
            else
                tkerr("Invalid expression after multiplication expression");
        }
        else
            tkerr("Invalid expression after + operation");
    }
    if (consume(SUB))
    {
        if (exprMul())
        {
            if (exprAddPrim())
            {
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
bool exprAdd()
{
    Token *start = iTk;
    if (exprMul())
    {
        if (exprAddPrim())
        {
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
bool exprRelPrim()
{
    Token *start = iTk;
    if (consume(LESS))
    {
        if (exprAdd())
        {
            if (exprRelPrim())
            {
                return true;
            }
            else
                tkerr("Invalid expression after add expression");
        }
        else
            tkerr("Invalid expression after < symbol");
    }
    if (consume(LESSEQ))
    {
        if (exprAdd())
        {
            if (exprRelPrim())
            {
                return true;
            }
            else
                tkerr("Invalid expression after add expression");
        }
        else
            tkerr("Invalid expression after <= symbol");
    }
    if (consume(GREATER))
    {
        if (exprAdd())
        {
            if (exprRelPrim())
            {
                return true;
            }
            else
                tkerr("Invalid expression after add expression");
        }
        else
            tkerr("Invalid expression after > symbol");
    }
    if (consume(GREATEREQ))
    {
        if (exprAdd())
        {
            if (exprRelPrim())
            {
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
bool exprRel()
{
    Token *start = iTk;
    if (exprAdd())
    {
        if (exprRelPrim())
        {
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
bool exprEqPrim()
{
    Token *start = iTk;
    if (consume(EQUAL))
    {
        if (exprRel())
        {
            if (exprEqPrim())
            {
                return true;
            }
            else
                tkerr("Invalid expression after rel expression");
        }
        else
            tkerr("Invalid expression after == symbol");
    }
    if (consume(NOTEQ))
    {
        if (exprRel())
        {
            if (exprEqPrim())
            {
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
bool exprEq()
{
    Token *start = iTk;
    if (exprRel())
    {
        if (exprEqPrim())
        {
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

bool exprAndPrim()
{
    Token *start = iTk;
    if (consume(AND))
    {
        if (exprEq())
        {
            if (exprAndPrim())
            {
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
bool exprAnd()
{
    Token *start = iTk;
    if (exprEq())
    {
        if (exprAndPrim())
        {
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
bool exprOrPrim()
{
    Token *start = iTk;
    if (consume(OR))
    {
        if (exprAnd())
        {
            if (exprOrPrim())
            {
                return true;
            }
            else
                tkerr("Invalid expression after and expression");
        }
        else
            tkerr("Invalid expression after || operator");
    }

    iTk = start;
    return true;
}
bool exprOr()
{
    Token *start = iTk;
    if (exprAnd())
    {
        if (exprOrPrim())
        {
            return true;
        }
        else
            tkerr("Invalid expression after and expression");
    }
    iTk = start;
    return false;
}
bool exprAssign()
{
    Token *start = iTk;
    if (exprUnary())
    {

        if (consume(ASSIGN))
        {

            if (exprAssign())
            {

                return true;
            }
            else
                tkerr("Invalid expression after = operation");
        }
    }
    iTk = start;
    if (exprOr())
    {

        return true;
    }
    iTk = start;
    return false;
}
bool expr()
{
    Token *start = iTk;
    if (exprAssign())
    {
        return true;
    }
    iTk = start;
    return false;
}

bool stm()
{
    Token *start = iTk;
    if (stmCompound())
    {
        return true;
    }
    if (consume(IF))
    {
        if (consume(LPAR))
        {
            if (expr())
            {
                if (consume(RPAR))
                {
                    if (stm())
                    {
                        if (consume(ELSE))
                        {
                            if (stm())
                            {
                            }
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
    if (consume(WHILE))
    {
        if (consume(LPAR))
        {
            if (expr())
            {
                if (consume(RPAR))
                {
                    if (stm())
                    {
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
    if (consume(RETURN))
    {
        if (expr())
        {
        }
        if (consume(SEMICOLON))
        {
            return true;
        }
        else
            tkerr("Missing ; on return statement");
    }

    if (expr())
    {
    }
    if (consume(SEMICOLON))
    {
        return true;
    }

    iTk = start;
    return false;
}
bool stmCompound()
{
    Token *start = iTk;
    if (consume(LACC))
    {

        for (;;)
        {
            if (varDef())
            {
            }
            else if (stm())
            {
            }
            else
                break;
        }
        if (consume(RACC))
        {
            return true;
        }
        else
            tkerr("Missing } on stmCompound expression");
    }
    iTk = start;
    return false;
}
bool fnParam()
{
    Token *start = iTk;
    if (typeBase())
    {
        if (consume(ID))
        {
            if (arrayDecl())
            {
            }
            return true;
        }
        else
            tkerr("Missing function parameter name");
    }
    iTk = start;
    return false;
}
bool fnDef()
{
    Token *start = iTk;
    if (typeBase())
    {
        if (consume(ID))
        {
            if (consume(LPAR))
            {
                if (fnParam())
                {
                    for (;;)
                    {
                        if (consume(COMMA))
                        {
                            if (fnParam())
                            {
                            }
                        }
                        else
                        {
                            break;
                        }
                    }
                }
                if (consume(RPAR))
                {
                    if (stmCompound())
                    {
                        return true;
                    }
                }
                else
                    tkerr("Missing ) on function definition");
            }
        }
        else
            tkerr("Missing function name");
    }
    if (consume(VOID))
    {
        if (consume(ID))
        {
            if (consume(LPAR))
            {
                if (fnParam())
                {
                    for (;;)
                    {
                        if (consume(COMMA))
                        {
                            if (fnParam())
                            {
                            }
                        }
                        else
                        {
                            break;
                        }
                    }
                }
                if (consume(RPAR))
                {
                    if (stmCompound())
                    {
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

bool exprPrimary()
{
    Token *start = iTk;
    if (consume(ID))
    {
        if (consume(LPAR))
        {
            if (expr())
            {
                for (;;)
                {
                    if (consume(COMMA))
                    {
                        if (expr())
                        {
                        }
                    }
                    else
                    {
                        break;
                    }
                }
            }
            if (consume(RPAR))
            {
            }
        }
        return true;
    }
    if (consume(INT))
    {
        return true;
    }
    if (consume(DOUBLE))
    {
        return true;
    }
    if (consume(CHAR))
    {
        return true;
    }
    if (consume(STRING))
    {
        return true;
    }
    if (consume(LPAR))
    {
        if (expr())
        {
            if (consume(RPAR))
            {
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
bool exprPostfixPrim()
{
    Token *start = iTk;
    if (consume(LBRACKET))
    {
        if (expr())
        {
            if (consume(RBRACKET))
            {
                if (exprPostfixPrim())
                {
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
    if (consume(DOT))
    {
        if (consume(ID))
        {
            if (exprPostfixPrim())
            {
                return true;
            }
            else
                tkerr("Invalid expression after id");
        }
        else
            tkerr("Invalid id after . operation");
    }

    iTk = start;
    return true;
    ;
}
bool exprPostfix()
{
    Token *start = iTk;
    if (exprPrimary())
    {
        if (exprPostfixPrim())
        {
            return true;
        }
    }
    iTk = start;
    return false;
}

// unit: ( structDef | fnDef | varDef )* END
// bool unit()
// {
//     for (;;)
//     {
//         if (structDef())
//         {
//         }
//         else if (fnDef())
//         {
//         }
//         else if (varDef())
//         {
//         }
//         else
//             break;
//     }
//     if (consume(END))
//     {
//         return true;
//     }
//     return false;
// }

char** unit()
{
    char** errors = NULL;
    int errorCount = 0;
    int errorCapacity = 0;

    for (;;)
    {
        if (structDef())
        {
            // Process struct definition
        }
        else if (fnDef())
        {
            // Process function definition
        }
        else if (varDef())
        {
            // Process variable definition
        }
        else
        {
            break;
        }
    }

    if (consume(END))
    {
        if (errorCount > 0)
        {
            errors = (char**)malloc((errorCount + 1) * sizeof(char*)); // Allocate memory for error messages
            for (int i = 0; i < errorCount; i++)
            {
                errors[i] = strdup("syntax error"); // Replace "syntax error" with the actual error message
            }
            errors[errorCount] = NULL; // Set the last element to NULL to indicate the end of messages
        }
        return errors;
    }

    // Add the error message to the errors array
    errors = (char**)realloc(errors, (errorCount + 1) * sizeof(char*)); // Increase the capacity of the errors array
    errors[errorCount] = strdup("Expected END token."); // Replace "Expected END token." with the actual error message
    errorCount++;

    return errors;
}


void parse(Token *tokens)
{
    iTk = tokens;
    char **errorMessages = unit();

    if (errorMessages != NULL)
    {
        int i = 0;
        while (errorMessages[i] != NULL)
        {
            printf("Error: %s\n", errorMessages[i]);
            i++;
        }
        free(errorMessages); // Free the dynamically allocated error messages
    }
    else
    {
        // Parsing completed without errors
    }
}

// void parse(Token *tokens)
// {
//     iTk = tokens;
//     if (!unit())
//         tkerr("syntax error");
// }
