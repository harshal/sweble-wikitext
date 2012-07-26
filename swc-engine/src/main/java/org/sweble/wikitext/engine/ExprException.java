package org.sweble.wikitext.engine;

public class ExprException extends Exception
{
    private static final long serialVersionUID = 4291838934235089281L;

    public ExprException(String message)
    {
        super(message);
    }

    public ExprException(String message, String parameter)
    {
        super(message + "|" + parameter) ;
    }
    
    public ExprException(String message, TokenType token)
    {
        super(message);
    }

}
