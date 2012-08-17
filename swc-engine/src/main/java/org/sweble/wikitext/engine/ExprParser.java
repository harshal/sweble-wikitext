package org.sweble.wikitext.engine;

import static java.math.BigDecimal.ONE;
import static java.math.BigDecimal.TEN;
import static java.math.BigDecimal.ZERO;
import static org.sweble.wikitext.engine.TokenType.EXPR_ABS;
import static org.sweble.wikitext.engine.TokenType.EXPR_AND;
import static org.sweble.wikitext.engine.TokenType.EXPR_ARCCOS;
import static org.sweble.wikitext.engine.TokenType.EXPR_ARCSINE;
import static org.sweble.wikitext.engine.TokenType.EXPR_ARCTAN;
import static org.sweble.wikitext.engine.TokenType.EXPR_CEIL;
import static org.sweble.wikitext.engine.TokenType.EXPR_CLOSE;
import static org.sweble.wikitext.engine.TokenType.EXPR_COSINE;
import static org.sweble.wikitext.engine.TokenType.EXPR_DIVIDE;
import static org.sweble.wikitext.engine.TokenType.EXPR_EQUALITY;
import static org.sweble.wikitext.engine.TokenType.EXPR_EXP;
import static org.sweble.wikitext.engine.TokenType.EXPR_EXPONENT;
import static org.sweble.wikitext.engine.TokenType.EXPR_FLOOR;
import static org.sweble.wikitext.engine.TokenType.EXPR_GREATER;
import static org.sweble.wikitext.engine.TokenType.EXPR_GREATEREQ;
import static org.sweble.wikitext.engine.TokenType.EXPR_LESS;
import static org.sweble.wikitext.engine.TokenType.EXPR_LESSEQ;
import static org.sweble.wikitext.engine.TokenType.EXPR_LN;
import static org.sweble.wikitext.engine.TokenType.EXPR_MINUS;
import static org.sweble.wikitext.engine.TokenType.EXPR_MOD;
import static org.sweble.wikitext.engine.TokenType.EXPR_NEGATIVE;
import static org.sweble.wikitext.engine.TokenType.EXPR_NOT;
import static org.sweble.wikitext.engine.TokenType.EXPR_NOTEQ;
import static org.sweble.wikitext.engine.TokenType.EXPR_OPEN;
import static org.sweble.wikitext.engine.TokenType.EXPR_OR;
import static org.sweble.wikitext.engine.TokenType.EXPR_PI;
import static org.sweble.wikitext.engine.TokenType.EXPR_PLUS;
import static org.sweble.wikitext.engine.TokenType.EXPR_POSITIVE;
import static org.sweble.wikitext.engine.TokenType.EXPR_POW;
import static org.sweble.wikitext.engine.TokenType.EXPR_ROUND;
import static org.sweble.wikitext.engine.TokenType.EXPR_SINE;
import static org.sweble.wikitext.engine.TokenType.EXPR_TANGENS;
import static org.sweble.wikitext.engine.TokenType.EXPR_TIMES;
import static org.sweble.wikitext.engine.TokenType.EXPR_TRUNC;

import java.math.BigDecimal;
import java.math.MathContext;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;

public class ExprParser
{
    private static class SafeDeque<T> extends ArrayDeque<T>
    {
        private static final long serialVersionUID = -5996172483565030277L;

        T popSafe()
        {
            if (isEmpty())
                return null;
            return pop();
        }
    };

    private static enum Expecting
    {
        EXPRESSION,
        OPERATOR
    };

    public static int MAX_STACK_SIZE = 100;

    public static final String EXPR_WHITE_CLASS = " \t\r\n";
    public static final String EXPR_NUMBER_CLASS = "0123456789.";
    public static final Pattern PAT_WORD = Pattern.compile("^[A-Za-z]*");

    public static final EnumMap<TokenType, Integer> precedence = new EnumMap<TokenType, Integer>(
            TokenType.class);
    public static final HashMap<String, TokenType> words = new HashMap<String, TokenType>();

    static
    {
        precedence.put(EXPR_NEGATIVE, 10);
        precedence.put(EXPR_POSITIVE, 10);
        precedence.put(EXPR_EXPONENT, 10);
        precedence.put(EXPR_SINE, 9);
        precedence.put(EXPR_COSINE, 9);
        precedence.put(EXPR_TANGENS, 9);
        precedence.put(EXPR_ARCSINE, 9);
        precedence.put(EXPR_ARCCOS, 9);
        precedence.put(EXPR_ARCTAN, 9);
        precedence.put(EXPR_EXP, 9);
        precedence.put(EXPR_LN, 9);
        precedence.put(EXPR_ABS, 9);
        precedence.put(EXPR_FLOOR, 9);
        precedence.put(EXPR_TRUNC, 9);
        precedence.put(EXPR_CEIL, 9);
        precedence.put(EXPR_NOT, 9);
        precedence.put(EXPR_POW, 8);
        precedence.put(EXPR_TIMES, 7);
        precedence.put(EXPR_DIVIDE, 7);
        precedence.put(EXPR_MOD, 7);
        precedence.put(EXPR_PLUS, 6);
        precedence.put(EXPR_MINUS, 6);
        precedence.put(EXPR_ROUND, 5);
        precedence.put(EXPR_EQUALITY, 4);
        precedence.put(EXPR_LESS, 4);
        precedence.put(EXPR_GREATER, 4);
        precedence.put(EXPR_LESSEQ, 4);
        precedence.put(EXPR_GREATEREQ, 4);
        precedence.put(EXPR_NOTEQ, 4);
        precedence.put(EXPR_AND, 3);
        precedence.put(EXPR_OR, 2);
        precedence.put(EXPR_PI, 0);
        precedence.put(EXPR_OPEN, -1);
        precedence.put(EXPR_CLOSE, -1);

        words.put("mod", EXPR_MOD);
        words.put("and", EXPR_AND);
        words.put("or", EXPR_OR);
        words.put("not", EXPR_NOT);
        words.put("round", EXPR_ROUND);
        words.put("div", EXPR_DIVIDE);
        words.put("e", EXPR_EXPONENT);
        words.put("sin", EXPR_SINE);
        words.put("cos", EXPR_COSINE);
        words.put("tan", EXPR_TANGENS);
        words.put("asin", EXPR_ARCSINE);
        words.put("acos", EXPR_ARCCOS);
        words.put("atan", EXPR_ARCTAN);
        words.put("exp", EXPR_EXP);
        words.put("ln", EXPR_LN);
        words.put("abs", EXPR_ABS);
        words.put("trunc", EXPR_TRUNC);
        words.put("floor", EXPR_FLOOR);
        words.put("ceil", EXPR_CEIL);
        words.put("pi", EXPR_PI);
    }

    private static String substr(String str, int start, int length)
    {
        if (start < 0)
        {
            start = str.length() + start;
        }
        if (length < 0)
        {
            length = str.length() + length;
        }
        return str.substring(start, Math.min(start+length, str.length()));
    }

    private static String substr(String str, int start)
    {
        if (start < 0)
        {
            start = str.length() + start;
        }
        return str.substring(start);
    }

    private static int strspn(String s, String mask, int start)
    {
        boolean failed = false;
        int ii = start;
        while (!failed && ii < s.length()) {
            int match = 0;
            for (int jj = 0; jj < mask.length(); jj++) {
                if (s.charAt(ii) == mask.charAt(jj)) {
                    match++;
                }
            }
            if (match == 0) {
                break;
            }
            ii++;
        }
        return ii - start;
    }

    private static int strpos(String haystack, char needle)
    {
        return haystack.indexOf(needle);
    }

    private static BigDecimal n(String s) throws ExprException
    {
        try
        {
            return new BigDecimal(s.trim());
        }
        catch (NumberFormatException e)
        {
            throw new ExprException("Error parsing float string:" +s + '|' + s.length());
        }
    }

    private static BigDecimal n(long l)
    {
        return new BigDecimal(l);
    }

    private static BigDecimal n(double d)
    {
        return new BigDecimal(d);
    }

    private static BigDecimal r(boolean b)
    {
        return b ? ONE : ZERO;
    }

    private static boolean isTrueExpr(BigDecimal bd)
    {
        return (bd.compareTo(ZERO) != 0);
    }

    private BigDecimal tenpow(int n)
    {
        if (n >= 0)
            return TEN.pow(n);
        else
            return TEN.divide(TEN.pow((-1*n)+1));
    }


    public String doExpression(String expr) throws ExprException
    {
        SafeDeque<BigDecimal> operands = new SafeDeque<BigDecimal>();
        SafeDeque<TokenType> operators = new SafeDeque<TokenType>();

        int p = 0;
        int end = expr.length();
        Expecting expect = Expecting.EXPRESSION;

        String name = "JunkName";
        String word;
        TokenType op, lastOp;
        char char1;
        while ( p < end )
        {
            //System.out.println("Foo " + expect);
            if ( operands.size() > MAX_STACK_SIZE || operators.size() > MAX_STACK_SIZE ) {
                throw new ExprException( "stack_exhausted" );
            }
            char1 = expr.charAt(p);
            String char2 = substr( expr, p, 2 );
            //System.out.println(char1);
            // Mega if-elseif-else construct
            // Only binary operators fall through for processing at the bottom, the rest
            // finish their processing and continue

            // First the unlimited length classes

            if ( -1 != strpos( EXPR_WHITE_CLASS, char1) ) {
                // Whitespace
                p += strspn( expr, EXPR_WHITE_CLASS, p );
                continue;
            } else if ( -1 != strpos( EXPR_NUMBER_CLASS, char1) ) {
                // Number
                if ( expect != Expecting.EXPRESSION) {
                    throw new ExprException( "unexpected_number" + char1 );
                }
                //System.out.println("n" + expect);
                // Find the rest of it
                int length = strspn( expr, EXPR_NUMBER_CLASS, p );
                // Convert it to float, silently removing double decimal points
                operands.push(n(substr( expr, p, length )));
                p += length;
                expect = Expecting.OPERATOR;
                //System.out.println("Continuing");
                continue;
            } else if ( Character.isLetter(char1) ) {
                // Word
                // Find the rest of it
                String remaining = substr( expr, p );
                Matcher m = PAT_WORD.matcher(remaining);
                if (!m.find())
                {
                    // This should be unreachable
                    throw new ExprException( "preg_match_failure" );
                }
                word = m.group().toLowerCase();
                p += word.length();

                // Interpret the word
                if ( !words.containsKey(word) ) {
                    throw new ExprException( "unrecognised_word", word );
                }
                op = words.get(word);
                if (op == EXPR_EXPONENT)
                {
                    //System.out.println(op + operands.toString() +operators);
                }
                switch( op ) {
                    // constant
                    case EXPR_EXPONENT:
                        if (expect != Expecting.EXPRESSION) {
                            break;
                        }
                        operands.push(n(Math.E));
                        expect = Expecting.OPERATOR;
                        continue;
                    case EXPR_PI:
                        if (expect != Expecting.EXPRESSION ) {
                            throw new ExprException( "unexpected_number" );
                        }
                        operands.push(n(Math.PI));
                        expect = Expecting.OPERATOR;
                        continue;
                        // Unary operator
                    case EXPR_NOT:
                    case EXPR_SINE:
                    case EXPR_COSINE:
                    case EXPR_TANGENS:
                    case EXPR_ARCSINE:
                    case EXPR_ARCCOS:
                    case EXPR_ARCTAN:
                    case EXPR_EXP:
                    case EXPR_LN:
                    case EXPR_ABS:
                    case EXPR_FLOOR:
                    case EXPR_TRUNC:
                    case EXPR_CEIL:
                        if ( expect != Expecting.EXPRESSION ) {
                            throw new ExprException( "unexpected_operator", word );
                        }
                        operators.push(op);
                        continue;
                }
                // Binary operator, fall through
                name = word;
            }

            // Next the two-character operators

            else if (char2.equals("<=") ) {
                name = char2;
                op = EXPR_LESSEQ;
                p += 2;
            } else if (char2.equals(">=") ) {
                name = char2;
                op = EXPR_GREATEREQ;
                p += 2;
            } else if ( char2.equals("<>") || char2.equals("!=")) {
                name = char2;
                op = EXPR_NOTEQ;
                p += 2;
            }

            // Finally the single-character operators

            else if ( char1 == '+' ) {
                ++p;
                if (expect == Expecting.EXPRESSION) {
                    // Unary plus
                    operators.push(EXPR_POSITIVE);
                    continue;
                } else {
                    // Binary plus
                    op = EXPR_PLUS;
                }
            } else if ( char1== '-' ) {
                ++p;
                if ( expect == Expecting.EXPRESSION  ) {
                    // Unary minus
                    operators.push(EXPR_NEGATIVE);
                    continue;
                } else {
                    // Binary minus
                    op = EXPR_MINUS;
                }
            } else if ( char1== '*' ) {
                name = String.valueOf(char1);
                op = EXPR_TIMES;
                ++p;
            } else if ( char1== '/' ) {
                name = String.valueOf(char1);
                op = EXPR_DIVIDE;
                ++p;
            } else if ( char1== '^' ) {
                name = String.valueOf(char1);
                op = EXPR_POW;
                ++p;
            } else if ( char1== '(' )  {
                if ( expect == Expecting.OPERATOR  ) {
                    throw new ExprException( "unexpected_operator", String.valueOf('('));
                }
                operators.push(EXPR_OPEN);
                ++p;
                continue;
            } else if ( char1 == ')' ) {
                lastOp = operators.peekFirst();
                while ( lastOp != null && lastOp != EXPR_OPEN ) {
                    doOperation( lastOp, operands );
                    operators.pop();
                    lastOp = operators.peekFirst();
                }
                if ( lastOp != null ) {
                    operators.popSafe();
                } else {
                    throw new ExprException( "unexpected_closing_bracket" );
                }
                expect = Expecting.OPERATOR;
                ++p;
                continue;
            } else if ( char1 == '=' ) {
                name = String.valueOf(char1);
                op = EXPR_EQUALITY;
                ++p;
            } else if ( char1== '<' ) {
                name = String.valueOf(char1);
                op = EXPR_LESS;
                ++p;
            } else if ( char1== '>' ) {
                name = String.valueOf(char1);
                op = EXPR_GREATER;
                ++p;
            } else {
                throw new ExprException( "unrecognised_punctuation", String.valueOf(char1));
            }
            //System.out.println("came out of big if");
            // Binary operator processing
            if (expect == Expecting.EXPRESSION ) {
                throw new ExprException( "unexpected_operator", name );
            }

            // Shunting yard magic
            lastOp = operators.peekFirst();
            while ( lastOp != null && (precedence.get(op) <= precedence.get(lastOp)) ) {
                doOperation( lastOp, operands );
                operators.pop();
                lastOp = operators.peekFirst();
            }
            operators.push(op);
            expect = Expecting.EXPRESSION;
            //System.out.println("Resetting to expression at end of loop");
        }

        // Finish off the operator array
        while (!operators.isEmpty())
        {
            op = operators.pop(); 
            if ( op == EXPR_OPEN ) {
                throw new ExprException( "unclosed_bracket" );
            }
            doOperation( op, operands );
        }

        return printNums(operands , "<br />\n" );
    }

    public static int round(double a) {
        return (int) (a < 0 ? Math.round(a - 0.0001) : Math.round(a));
    }

    private String printNums(Collection<BigDecimal> a, String sep)
    {
        ArrayList<String> strs = new ArrayList<String>(a.size());
        for (BigDecimal bd : a)
        {
            //System.out.println(bd.round(MathContext.DECIMAL32));
            //bd.setScale(0);
            strs.add(bd.round(MathContext.DECIMAL32).stripTrailingZeros().toPlainString());
        }
        return StringUtils.join(strs, sep);
    }

    private void doOperation(TokenType op, SafeDeque<BigDecimal> stack) throws ExprException
    {
        BigDecimal left;
        BigDecimal right;
        BigDecimal arg;
        switch ( op ) {
            case EXPR_NEGATIVE:
                if ( stack.size() < 1 ) {
                    throw new ExprException("missing_operand", op);
                }
                arg = stack.popSafe();
                stack.push(arg.negate());
                break;
            case EXPR_POSITIVE:
                if ( stack.size() < 1 ) {
                    throw new ExprException("missing_operand", op);
                }
                break;
            case EXPR_TIMES:
                if ( stack.size() < 2 ) {
                    throw new ExprException("missing_operand", op);
                }
                right = stack.popSafe();
                left = stack.popSafe();
                stack.push(left.multiply(right));
                break;
            case EXPR_DIVIDE:
                if ( stack.size() < 2 ) {
                    throw new ExprException("missing_operand", op);
                }
                right = stack.popSafe();
                left = stack.popSafe();
                if ( right.intValue() == 0 ) {
                    throw new ExprException("division_by_zero", op);
                }
                // XXX : Is this right?
                stack.push(left.divide(right, MathContext.DECIMAL128));
                break;
            case EXPR_MOD:
                if ( stack.size() < 2 ) {
                    throw new ExprException("missing_operand", op);
                }
                right = stack.popSafe();
                left = stack.popSafe();
                if ( right.intValue() == 0 ) {
                    throw new ExprException("division_by_zero", op);
                }
                stack.push(n(left.intValue() % right.intValue()));
                break;
            case EXPR_PLUS:
                if ( stack.size() < 2 ) {
                    throw new ExprException("missing_operand", op);
                }
                right = stack.popSafe();
                left = stack.popSafe();
                stack.push(left.add(right));
                break;
            case EXPR_MINUS:
                if ( stack.size() < 2 ) {
                    throw new ExprException("missing_operand", op);
                }
                right = stack.popSafe();
                left = stack.popSafe();
                stack.push(left.subtract(right));
                break;
            case EXPR_AND:
                if ( stack.size() < 2 ) {
                    throw new ExprException("missing_operand", op);
                }
                right = stack.popSafe();
                left = stack.popSafe();
                stack.push(isTrueExpr(left) && isTrueExpr(right) ? ONE : ZERO);
                break;
            case EXPR_OR:
                if ( stack.size() < 2 ) {
                    throw new ExprException("missing_operand", op);
                }
                right = stack.popSafe();
                left = stack.popSafe();
                stack.push(isTrueExpr(left) || isTrueExpr(right) ? ONE : ZERO);
                break;
            case EXPR_EQUALITY:
                if ( stack.size() < 2 ) {
                    throw new ExprException("missing_operand", op);
                }
                right = stack.popSafe().round(MathContext.DECIMAL32);
                left = stack.popSafe().round(MathContext.DECIMAL32);
                stack.push(r(left.compareTo(right) == 0));
                break;
            case EXPR_NOT:
                if ( stack.size() < 1 ) {
                    throw new ExprException("missing_operand", op);
                }
                arg = stack.popSafe();
                stack.push(r(arg.compareTo(ZERO) == 0));
                break;
            case EXPR_ROUND:
                if ( stack.size() < 2 ) {
                    throw new ExprException("missing_operand", op);
                }
                int digits = stack.popSafe().intValue();
                BigDecimal multipied = stack.popSafe().multiply(tenpow(digits));
                //System.out.println("** M val" + multipied + ":" + (multipied.floatValue()-0.001f) + ":" + Math.round(multipied.floatValue()-0.00001f));
                BigDecimal rounded = n(round(multipied.doubleValue()));
                //System.out.println("** Rounded val" + rounded);
                BigDecimal b = rounded.divide(tenpow(digits));
                stack.push(b);
                break;
            case EXPR_LESS:
                if ( stack.size() < 2 ) {
                    throw new ExprException("missing_operand", op);
                }
                right = stack.popSafe();
                left = stack.popSafe();
                stack.push(r(left.compareTo(right) < 0));
                break;
            case EXPR_GREATER:
                if ( stack.size() < 2 ) {
                    throw new ExprException("missing_operand", op);
                }
                right = stack.popSafe();
                left = stack.popSafe();
                stack.push(r(left.compareTo(right) > 0));
                break;
            case EXPR_LESSEQ:
                if ( stack.size() < 2 ) {
                    throw new ExprException("missing_operand", op);
                }
                right = stack.popSafe();
                left = stack.popSafe();
                stack.push(r(left.compareTo(right) <= 0));
                break;
            case EXPR_GREATEREQ:
                if ( stack.size() < 2 ) {
                    throw new ExprException("missing_operand", op);
                }
                right = stack.popSafe();
                left = stack.popSafe();
                stack.push(r(left.compareTo(right) >= 0));
                break;
            case EXPR_NOTEQ:
                if ( stack.size() < 2 ) {
                    throw new ExprException("missing_operand", op);
                }
                right = stack.popSafe();
                left = stack.popSafe();
                stack.push(r(left.compareTo(right) != 0));
                break;
            case EXPR_EXPONENT:
                if ( stack.size() < 2 ) {
                    throw new ExprException("missing_operand", op);
                }
                right = stack.popSafe();
                left = stack.popSafe();
                stack.push(left.multiply(TEN.pow(right.intValue(), MathContext.DECIMAL128)));
                break;
//                case EXPR_SINE:
//                    if ( stack.size() < 1 ) {
//                        throw new ExprException("missing_operand", op);
//                    }
//                    $arg = stack.popSafe();
//                    stack.push(sin( $arg );
//                    break;
//                case EXPR_COSINE:
//                    if ( stack.size() < 1 ) {
//                        throw new ExprException("missing_operand", op);
//                    }
//                    $arg = stack.popSafe();
//                    stack.push(cos( $arg );
//                    break;
//                case EXPR_TANGENS:
//                    if ( stack.size() < 1 ) {
//                        throw new ExprException("missing_operand", op);
//                    }
//                    $arg = stack.popSafe();
//                    stack.push(tan( $arg );
//                    break;
//                case EXPR_ARCSINE:
//                    if ( stack.size() < 1 ) {
//                        throw new ExprException("missing_operand", op);
//                    }
//                    $arg = stack.popSafe();
//                    if ( $arg < -1 || $arg > 1 ) {
//                        throw new ExprException("invalid_argument", op);;
//                    }
//                    stack.push(asin( $arg );
//                    break;
//                case EXPR_ARCCOS:
//                    if ( stack.size() < 1 ) {
//                        throw new ExprException("missing_operand", op);
//                    }
//                    $arg = stack.popSafe();
//                    if ( $arg < -1 || $arg > 1 ) {
//                        throw new ExprException("invalid_argument", op);;
//                    }
//                    stack.push(acos( $arg );
//                    break;
//                case EXPR_ARCTAN:
//                    if ( stack.size() < 1 ) {
//                        throw new ExprException("missing_operand", op);
//                    }
//                    $arg = stack.popSafe();
//                    stack.push(atan( $arg );
//                    break;
//                case EXPR_EXP:
//                    if ( stack.size() < 1 ) {
//                        throw new ExprException("missing_operand", op);
//                    }
//                    arg = stack.popSafe();
//                    //this is wrong
//                    stack.push(TEN.pow(arg.intValue()));
//                    break;
//                case EXPR_LN:
//                    if ( stack.size() < 1 ) {
//                        throw new ExprException("missing_operand", op);
//                    }
//                    $arg = stack.popSafe();
//                    if ( $arg <= 0 ) {
//                        throw new ExprException("invalid_argument_ln", op);;
//                    }
//                    stack.push(log( $arg );
//                    break;
//                case EXPR_ABS:
//                    if ( stack.size() < 1 ) {
//                        throw new ExprException("missing_operand", op);
//                    }
//                    arg = stack.popSafe();
//                    stack.push(arg.abs());
//                    break;
//                case EXPR_FLOOR:
//                    if ( stack.size() < 1 ) {
//                        throw new ExprException("missing_operand", op);
//                    }
//                    $arg = stack.popSafe();
//                    stack.push(floor( $arg );
//                    break;
//                case EXPR_TRUNC:
//                    if ( stack.size() < 1 ) {
//                        throw new ExprException("missing_operand", op);
//                    }
//                    $arg = stack.popSafe();
//                    stack.push((int)$arg;
//                    break;
//                case EXPR_CEIL:
//                    if ( stack.size() < 1 ) {
//                        throw new ExprException("missing_operand", op);
//                    }
//                    $arg = stack.popSafe();
//                    stack.push(ceil( $arg );
//                    break;
//                case EXPR_POW:
//                    if ( stack.size() < 2 ) {
//                        throw new ExprException("missing_operand", op);
//                    }
//                    right = stack.popSafe();
//                    left = stack.popSafe();
//                    if ( false === ( $stack[] = pow( left, $right ) ) ) {
//                        throw new ExprException("division_by_zero", op);
//                    }
//                    break;
            default:
                // Should be impossible to reach here.
                throw new ExprException("unknown_error " + op);
        }
    }

}
