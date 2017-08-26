package edu.binghamton.cs571;

public class UglyRegexpParser {

  Token _lookahead;
  Scanner _scanner;

  UglyRegexpParser(Scanner scanner) {
    _scanner = scanner;
    _lookahead = _scanner.nextToken();
  }



  /** parse a sequence of lines containing ugly-regexp's; for each
   *  ugly regexp print out the corresponding standard regexp.
   *  If there is an error, print diagnostic and continue with
   *  next line.
   *  
   *  	
   *  CFG to trasnlate given Ugly Regexp chars(x1...xn)
   *  into standard regexp is as follows; 	
   *
   *  exp         -> term expRest 
   *  expRest     -> . term expRest | empty
   *  term        -> factor termRest 
   *  termRest    -> + factor termRest | empty 
   *  factor      -> * factor | simple
   *  simple      -> ( exp ) | restSimple
   *  restSimple  -> chars( t )
   *  t 	  -> ASCII symbols | ASCII symbols, t
   *
   *
   *	
   */

  public void parse() {
    while (_lookahead.kind != Token.Kind.EOF) {
      try {
        String out = uglyRegexp();
        if (check(Token.Kind.NL)) System.out.println(out);
        match(Token.Kind.NL);
      }
      catch (ParseException e) {
        System.err.println(e.getMessage());
        while (_lookahead.kind != Token.Kind.NL) {
          _lookahead = _scanner.nextToken();
        }
        _lookahead = _scanner.nextToken();
      }
    }
  }

  /** Return standard syntax regexp corresponding to ugly-regexp
   *  read from _scanner.
   */
  //IMPLEMENT THIS FUNCTION and any necessary functions it may call.

  /**
  * start of the expression symbol
  */

  private String uglyRegexp() {
   	return exp();
  }


  /**
   * exp -> term expRest      
   */

  private String exp() {
	return expRest(term());
  }

  /**
  * expRest -> . term expRest | empty
  */
  private String expRest(String valueSoFar) {
	if(check(Token.Kind.CHAR, ".")) { // check for the actual lexeme matches with the enum kind
		match(Token.Kind.CHAR, _lookahead.lexeme);
		return expRest("(" + valueSoFar + term()+ ")");
	} else {
	}	return valueSoFar;
  }	

 /**
 *  term -> factor termRest
 */

  private String term() {
	return termRest(factor());
  }

 
  /**
  * termRest -> + factor termRest | empty
  * valueSoFar :
  *  		string containing value expression 
  */
  private String termRest(String valueSoFar) {
	if(check(Token.Kind.CHAR, "+")) {
		match(Token.Kind.CHAR, _lookahead.lexeme); // matches for the particular enum kind and if matches then get the next token
		return termRest("(" + valueSoFar + "|" + factor()+ ")");
	} else {
		return valueSoFar;
	}
  }
  
  /**
  * factor -> * factor | simple
  */
  private String factor() {
	if(check(Token.Kind.CHAR, "*")) {
		match(Token.Kind.CHAR, _lookahead.lexeme);
		return factor() + "*";
	} else {
		return simple();
	}
  }
  
  /**
  * simple -> ( exp ) | restSimple
  */
  private String simple() {
	if(check(Token.Kind.CHAR, "(")) {
		match(Token.Kind.CHAR, _lookahead.lexeme);
		String exp_result = exp();
		if(check(Token.Kind.CHAR, ")")) {
			match(Token.Kind.CHAR, _lookahead.lexeme);
		}
		else {
			match(Token.Kind.CHAR, ")"); // error condition
		}
		return "(" + exp_result + ")";
	} else {
		return restSimple();
	}
  }

  /**
  * restSimple -> chars( t )
  */
  private String restSimple() {

	if(check(Token.Kind.CHARS, "chars")) {
		match(Token.Kind.CHARS, _lookahead.lexeme);
		if(check(Token.Kind.CHAR, "(")) {
			match(Token.Kind.CHAR, _lookahead.lexeme);
			if(check(Token.Kind.CHAR, _lookahead.lexeme))
			{
				String exp_result = T(_lookahead.lexeme);
				if(check(Token.Kind.CHAR, ")")) {
				match(Token.Kind.CHAR, _lookahead.lexeme);
				}
				else {
				match(Token.Kind.CHAR,")"); // to specify error condition of why it is occurring
				}
				return "[" + exp_result + "]";
			} else {
			match(Token.Kind.CHAR, "CHAR"); // error condition
			}
		} else {
		 	match(Token.Kind.CHAR, "(");
		}
	} else {
			match(Token.Kind.CHARS, "chars"); // error condition
	}
	return "";
  }

  
   /**
   *	
   * t -> ASCII symbols | ASCII symbols, t
   * valueSoFar :
   *		  string containing value expression 
   */
   private String T(String valueSoFar) {
	String exp_terminal = ""; // to store next terminal after first meta character	
	match(Token.Kind.CHAR, _lookahead.lexeme);
	if(check(Token.Kind.CHAR, ",")) {
		match(Token.Kind.CHAR, _lookahead.lexeme);
		exp_terminal  = _lookahead.lexeme;
		return T(valueSoFar + quote(exp_terminal)); // quote() is used to add back slash to specific character
	} else {
		return quote(valueSoFar);
	}
  }


  //Utility functions which may be useful for parsing or translation

  /** Return s with first char escaped using a '\' if it is
   * non-alphanumeric.
   */
  private static String quote(String s) {
    return (Character.isLetterOrDigit(s.charAt(0))) ? s : "\\" + s;
  }

  /** Return true iff _lookahead.kind is equal to kind. */
  private boolean check(Token.Kind kind) {
    return check(kind, null);
  }

  /** Return true iff lookahead kind and lexeme are equal to
   *  corresponding args.  Note that if lexeme is null, then it is not
   *  used in the match.
   */
  private boolean check(Token.Kind kind, String lexeme) {
    return (_lookahead.kind == kind &&
            (lexeme == null || _lookahead.lexeme.equals(lexeme)));
  }

  /** If lookahead kind is equal to kind, then set lookahead to next
   *  token; else throw a ParseException.
   */
  private void match(Token.Kind kind) {
    match(kind, null);
  }

  /** If lookahead kind and lexeme are not equal to corresponding
   *  args, then set lookahead to next token; else throw a
   *  ParseException.  Note that if lexeme is null, then it is
   *  not used in the match.
   */
  private void match(Token.Kind kind, String lexeme) {
    if (check(kind, lexeme)) {
      _lookahead = _scanner.nextToken();
    }
    else {
      String expected = (lexeme == null) ? kind.toString() : lexeme;
      String message = String.format("%s: syntax error at '%s', expecting '%s'",
                                     _lookahead.coords, _lookahead.lexeme,
                                     expected);
      throw new ParseException(message);
    }
  }

  private static class ParseException extends RuntimeException {
    ParseException(String message) {
      super(message);
    }
  }


  /** main program: parses and translates ugly-regexp's contained in
   *  the file specified by it's single command-line argument.
   */
  public static void main(String[] args) {
    if (args.length != 1) {
      System.err.format("usage: java %s FILENAME\n",
                        UglyRegexpParser.class.getName());
      System.exit(1);
    }
    Scanner scanner =
      ("-".equals(args[0])) ? new Scanner() : new Scanner(args[0]);
    (new UglyRegexpParser(scanner)).parse();
  }


}
