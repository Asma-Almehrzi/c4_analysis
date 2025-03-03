// c4.c - C in four functions

// char, int, and pointer types
// if, while, return, and expression statements
// just enough features to allow self-compilation and a bit more

// Written by Robert Swierczek
//Annotated by:Asma

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <fcntl.h>
#define int long long // Use long long for all integers to support large memory space

// Global pointers for parsing and memory management
char *p, *lp, *data; // p: current position in source code, lp: last position, data: data section pointer
// data/bss pointer

// Global integer pointers for code emission and symbol handling 
int *e, *le,  // current position in emitted code
    *id,      // currently parsed identifier
    *sym;     // symbol table (simple list of identifiers)

// Global variables for tokenization and parsing
int tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line,     // current line number
    src,      // print source and assembly flag
    debug;    // print executed instructions

// Token definitions
// These represent different types of tokens used in the compiler.
// Operators are assigned higher values to establish precedence.
enum {
    Num = 128, // Numeric literals (e.g., 123, 0xFF)
    Fun,       // Function identifier
    Sys,       // System function call (e.g., printf, malloc)
    Glo,       // Global variable
    Loc,       // Local variable
    Id,        // General identifier (variable, function name, etc.)
  
    // Keywords
    Char,      // 'char' type
    Else,      // 'else' keyword
    Enum,      // 'enum' keyword
    If,        // 'if' statement
    Int,       // 'int' type
    Return,    // 'return' keyword
    Sizeof,    // 'sizeof' operator
    While,     // 'while' loop
  
    // Operators and precedence levels
    Assign,    // Assignment '='
    Cond,      // Ternary conditional operator '? :'
    Lor,       // Logical OR '||'
    Lan,       // Logical AND '&&'
    Or,        // Bitwise OR '|'
    Xor,       // Bitwise XOR '^'
    And,       // Bitwise AND '&'
    Eq,        // Equality comparison '=='
    Ne,        // Not equal '!='
    Lt,        // Less than '<'
    Gt,        // Greater than '>'
    Le,        // Less than or equal '<='
    Ge,        // Greater than or equal '>='
    Shl,       // Bitwise shift left '<<'
    Shr,       // Bitwise shift right '>>'
    Add,       // Addition '+'
    Sub,       // Subtraction '-'
    Mul,       // Multiplication '*'
    Div,       // Division '/'
    Mod,       // Modulo '%'
    Inc,       // Increment '++'
    Dec,       // Decrement '--'
    Brak       // Brackets '[]' (array indexing)
};
  
// Virtual Machine (VM) opcodes
// These represent instructions executed by the stack-based virtual machine.
enum { 
    LEA,  // Load effective address
    IMM,  // Load immediate value
    JMP,  // Unconditional jump
    JSR,  // Jump to subroutine
    BZ,   // Branch if zero
    BNZ,  // Branch if non-zero
    ENT,  // Function prologue (stack frame setup)
    ADJ,  // Adjust stack pointer
    LEV,  // Function epilogue (stack frame cleanup)
    LI,   // Load integer from memory
    LC,   // Load character from memory
    SI,   // Store integer in memory
    SC,   // Store character in memory
    PSH,  // Push value onto stack
  
    // Arithmetic and logical operations
    OR,   // Bitwise OR
    XOR,  // Bitwise XOR
    AND,  // Bitwise AND
    EQ,   // Equal comparison
    NE,   // Not equal comparison
    LT,   // Less than comparison
    GT,   // Greater than comparison
    LE,   // Less than or equal comparison
    GE,   // Greater than or equal comparison
    SHL,  // Bitwise shift left
    SHR,  // Bitwise shift right
    ADD,  // Addition
    SUB,  // Subtraction
    MUL,  // Multiplication
    DIV,  // Division
    MOD,  // Modulo
  
    // System functions
    OPEN,  // Open file
    READ,  // Read from file
    CLOS,  // Close file
    PRTF,  // Print formatted output
    MALC,  // Allocate memory (malloc)
    FREE,  // Free allocated memory
    MSET,  // Memory set (memset)
    MCMP,  // Memory compare (memcmp)
    EXIT   // Exit program
};
  
// Variable types
// These are the basic data types handled by the compiler.
enum { 
    CHAR,  // Character type (1 byte)
    INT,   // Integer type (4 bytes on most systems)
    PTR    // Pointer type (stores addresses, equivalent to INT size)
};
  
// Identifier structure offsets
// Instead of using a struct, identifiers are stored as an array of integers.
// These offsets define the fields used to store identifier attributes.
enum { 
    Tk,     // Token type (e.g., Id, Fun, Num)
    Hash,   // Hash value for quick lookups
    Name,   // Pointer to the identifier's name
    Class,  // Storage class (Local, Global, System)
    Type,   // Data type (CHAR, INT, PTR)
    Val,    // Value (for constants) or function address
    HClass, // Previous class (for symbol table management)
    HType,  // Previous type (for symbol table management)
    HVal,   // Previous value (for symbol table management)
    Idsz    // Total size of identifier entry
};
  

// Function to get the next token
// This function scans the source code and extracts tokens for parsing.
void next(){
  char *pp; // Pointer for tracking identifier start position

  while (tk = *p) { // Read the next character from the source code
    ++p;
    
    if (tk == '\n') { // Handle new lines
      if (src){ // If source tracing is enabled, print the line
        printf("%lld: %.*s", line, (int)(p - lp), lp);
        lp = p; // Update last processed position
        
        // Print generated instructions (debugging feature)
        while (le < e) {
          printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                           "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                           "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[*++le * 5]);
          if (*le <= ADJ) printf(" %lld\n", *++le); else printf("\n");
        }
      }
      ++line; // Increase line count for error reporting
    }
    
    else if (tk == '#') { // Handle preprocessor directives (ignored)
        while (*p != 0 && *p != '\n') ++p; // Skip until end of line
    }
    
    /* Process Identifiers and Keywords */
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {
        // Identifiers start with a letter (a-z, A-Z) or an underscore (_)
        // Keywords are processed the same way as identifiers.
    
        pp = p - 1; // Store the starting position of the identifier
    
        /* Continue scanning while characters are valid identifier characters */
        // Compute hash value for quick lookup (avoids slow string comparisons)
        while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
            tk = tk * 147 + *p++; // Hash function for efficient identifier lookup
        tk = (tk << 6) + (p - pp); 
    
        /* Explanation of Hashing:
           - Instead of storing full names and performing slow string comparisons,
             this method converts identifiers into unique numerical values.
           - Using a prime number (147) helps distribute values more evenly,
             reducing hash collisions and improving lookup performance.
           - The shift operation `tk = (tk << 6) + (p - pp);` further refines the hash 
             by incorporating the length of the identifier into the hash value.
        */
        
        id = sym; // Start at the beginning of the symbol table
    
        /* Search for the identifier in the symbol table */
        while (id[Tk]) { // Look for an existing identifier
            // Compare hashed value with stored identifiers in the symbol table
            if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { 
                tk = id[Tk]; // If found, assign the stored token type
                return; 
            }
            id = id + Idsz; // Move to the next entry in the symbol table
        }
    
        /* If identifier is new, store it in the symbol table */
        id[Name] = (int)pp; // Store pointer to the identifier name
        id[Hash] = tk; // Store the computed hash value
        tk = id[Tk] = Id; // Mark it as an identifier token
        return;
    }
    
    else if (tk >= '0' && tk <= '9') {
        // Handle Numeric Literals (Decimal, Hexadecimal, Octal)
    
        /* Process decimal numbers */
        if (ival = tk - '0') { 
            while (*p >= '0' && *p <= '9') 
                ival = ival * 10 + *p++ - '0'; 
        }
    
        /* Process hexadecimal numbers (0x or 0X) */
        else if (*p == 'x' || *p == 'X') {
            // Move past the 'x' or 'X' character
            while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
                // Convert hexadecimal character to integer value
                ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
        }
    
        /* Process octal numbers (numbers that start with 0) */
        else { 
            while (*p >= '0' && *p <= '7') 
                ival = ival * 8 + *p++ - '0'; 
        }
    
        tk = Num; // Mark token as a numeric literal
        return;
    }
    
    /* Handle Comments */
    else if (tk == '/') {
        if (*p == '/') { 
            // Single-line comment: Skip until end of line
            ++p;
            while (*p != 0 && *p != '\n') ++p;
        }
        else {
            tk = Div; // If it's not a comment, treat '/' as a division operator
            return;
        }
    }
    
    /* Handle Character and String Literals */
    else if (tk == '\'' || tk == '"') {
        pp = data; // Store the start position of the string
    
        while (*p != 0 && *p != tk) { // Read until closing quote
            if ((ival = *p++) == '\\') { // Handle escape sequences
                if ((ival = *p++) == 'n') 
                    ival = '\n'; // Convert `\n` escape to newline character
            }
            if (tk == '"') *data++ = ival; // Store character in string buffer
        }
    
        ++p; // Move past the closing quote
        if (tk == '"') ival = (int)pp; // If it's a string, store pointer to data
        else tk = Num; // If it's a character literal, treat it as a number
    
        return;
    }
    
    // Handle operators and special characters
    else if (tk == '=') { 
        if (*p == '=') { 
            ++p; tk = Eq; // Handle equality comparison (==)
        } 
        else tk = Assign; // Handle assignment (=)
        return; 
    }

    else if (tk == '+') { 
        if (*p == '+') { 
            ++p; tk = Inc; // Handle increment operator (++)
        } 
        else tk = Add; // Handle addition (+)
        return; 
    }

    else if (tk == '-') { 
        if (*p == '-') { 
            ++p; tk = Dec; // Handle decrement operator (--)
        } 
        else tk = Sub; // Handle subtraction (-)
        return; 
    }

    else if (tk == '!') { 
        if (*p == '=') { 
            ++p; tk = Ne; // Handle inequality (!=)
        } 
        return; 
    }

    else if (tk == '<') { 
        if (*p == '=') { 
            ++p; tk = Le; // Handle less than or equal (<=)
        } 
        else if (*p == '<') { 
            ++p; tk = Shl; // Handle bitwise left shift (<<)
        } 
        else tk = Lt; // Handle less than (<)
        return; 
    }

    else if (tk == '>') { 
        if (*p == '=') { 
            ++p; tk = Ge; // Handle greater than or equal (>=)
        } 
        else if (*p == '>') { 
            ++p; tk = Shr; // Handle bitwise right shift (>>)
        } 
        else tk = Gt; // Handle greater than (>)
        return; 
    }

    else if (tk == '|') { 
        if (*p == '|') { 
            ++p; tk = Lor; // Handle logical OR (||)
        } 
        else tk = Or; // Handle bitwise OR (|)
        return; 
    }

    else if (tk == '&') { 
        if (*p == '&') { 
            ++p; tk = Lan; // Handle logical AND (&&)
        } 
        else tk = And; // Handle bitwise AND (&)
        return; 
    }

    else if (tk == '^') { 
        tk = Xor; // Handle bitwise XOR (^)
        return; 
    }

    else if (tk == '%') { 
        tk = Mod; // Handle modulo operator (%)
        return; 
    }

    else if (tk == '*') { 
        tk = Mul; // Handle multiplication (*)
        return; 
    }

    else if (tk == '[') { 
        tk = Brak; // Handle array indexing operator ([ ])
        return; 
    }

    else if (tk == '?') { 
        tk = Cond; // Handle ternary conditional operator ( ? : )
        return; 
    }

    // Handle special characters without further parsing
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || 
            tk == ']' || tk == ',' || tk == ':') return;

    }
// Function to parse expressions
// This function is responsible for evaluating expressions using "precedence climbing"
// (also known as "top-down operator precedence"). 
// It supports various operators, function calls, and type casting.
void expr(int lev){ 
    int t, *d; // Temporary variables for expression processing
  
    if (!tk) { 
      // Handle unexpected end of expression (EOF)
      printf("%lld: unexpected eof in expression\n", line); 
      exit(-1);
    } 
    
    // Handle numeric literals (integers)
    else if (tk == Num) { 
      *++e = IMM; // Load immediate value
      *++e = ival; // Store numeric value
      next(); // Advance to next token
      ty = INT; // Set type to integer
    } 
    
    // Handle string literals
    else if (tk == '"') { 
      *++e = IMM; // Load immediate value
      *++e = ival; // Store address of string
      next();
      
      // Allow consecutive string literals (e.g., "hello" "world" becomes "helloworld")
      while (tk == '"') next();  
  
      // Align data pointer to word boundary for proper memory access
      data = (char *)((int)data + sizeof(int) & -sizeof(int)); 
      ty = PTR; // Set type to pointer
    } 
    
    // Handle sizeof operator
    else if (tk == Sizeof) { 
      next(); 
      if (tk == '(') next(); 
      else { 
        printf("%lld: open paren expected in sizeof\n", line); 
        exit(-1); 
      }
  
      ty = INT; // Default type is integer
  
      // Identify type (int or char)
      if (tk == Int) next(); 
      else if (tk == Char) { 
        next(); 
        ty = CHAR; 
      }
  
      // Handle pointer types (e.g., sizeof(int*) or sizeof(char**))
      while (tk == Mul) { 
        next(); 
        ty = ty + PTR; 
      }
  
      if (tk == ')') next(); 
      else { 
        printf("%lld: close paren expected in sizeof\n", line); 
        exit(-1); 
      }
  
      // Compute the size based on type
      *++e = IMM; 
      *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);
      ty = INT; // sizeof always returns an integer
    } 
    
    // Handle identifiers (variables and function calls)
    else if (tk == Id) { 
      d = id; // Store identifier
      next();
  
      // Handle function calls
      if (tk == '(') { 
        next();
        t = 0;
  
        // Process function arguments
        while (tk != ')') { 
          expr(Assign); 
          *++e = PSH; // Push argument onto stack
          ++t; 
          if (tk == ',') next(); // Handle multiple arguments
        }
  
        next(); // Consume closing parenthesis
  
        // Handle system calls and user-defined functions
        if (d[Class] == Sys) *++e = d[Val]; // System function call
        else if (d[Class] == Fun) { 
          *++e = JSR; // Jump to function address
          *++e = d[Val]; 
        } 
        else { 
          printf("%lld: bad function call\n", line); 
          exit(-1); 
        }
  
        // Adjust stack after function call (clean up arguments)
        if (t) { 
          *++e = ADJ; 
          *++e = t; 
        } 
  
        ty = d[Type]; // Set return type of function
      } 
      
      // Handle numeric constants
      else if (d[Class] == Num) { 
        *++e = IMM; 
        *++e = d[Val]; 
        ty = INT; 
      } 
      
      // Handle variables
      else { 
        if (d[Class] == Loc) { 
          *++e = LEA; // Load effective address
          *++e = loc - d[Val]; // Compute local variable offset
        } 
        else if (d[Class] == Glo) { 
          *++e = IMM; // Load global variable address
          *++e = d[Val]; 
        } 
        else { 
          printf("%lld: undefined variable\n", line); 
          exit(-1); 
        } 
  
        // Load value from memory (support for char and int types)
        *++e = ((ty = d[Type]) == CHAR) ? LC : LI;
      }
    }
  
    else if (tk == '(') { // Handle parentheses
        next();
        if (tk == Int || tk == Char) { // Handle type casting (e.g., (int) x or (char) y)
          t = (tk == Int) ? INT : CHAR; // Determine the type being casted to
          next();
          
          // Handle pointer types (e.g., (int*) ptr)
          while (tk == Mul) { 
            next(); 
            t = t + PTR; // Increase pointer level
          } 
          
          // Ensure closing parenthesis
          if (tk == ')') next(); 
          else { 
            printf("%lld: bad cast\n", line); 
            exit(-1); 
          }
    
          expr(Inc); // Process the expression being cast
          ty = t; // Set type to the casted type
        }
        else { 
          // Regular parenthesized expression (e.g., (a + b))
          expr(Assign);
          
          if (tk == ')') next(); 
          else { 
            printf("%lld: close paren expected\n", line); 
            exit(-1); 
          }
        }
      }
      
      else if (tk == Mul) { // Handle pointer dereference (e.g., *ptr)
        next(); 
        expr(Inc);
        
        // Ensure the type supports dereferencing
        if (ty > INT) ty = ty - PTR; 
        else { 
          printf("%lld: bad dereference\n", line); 
          exit(-1); 
        }
        
        *++e = (ty == CHAR) ? LC : LI; // Load character or integer value
      }
      
      else if (tk == And) { // Handle address-of operator (e.g., &var)
        next(); 
        expr(Inc);
        
        // Ensure valid lvalue (must be a variable or memory location)
        if (*e == LC || *e == LI) --e; 
        else { 
          printf("%lld: bad address-of\n", line); 
          exit(-1); 
        }
        
        ty = ty + PTR; // Convert type to pointer
      }
      
      else if (tk == '!') { // Handle logical NOT (!x)
        next(); 
        expr(Inc); 
        *++e = PSH; // Push value onto stack
        *++e = IMM; // Load immediate 0
        *++e = 0; 
        *++e = EQ; // Compare with zero (if x == 0, return 1; else return 0)
        ty = INT; // Result is always an integer (boolean)
      } 
      
      else if (tk == '~') { // Handle bitwise NOT (~x)
        next(); 
        expr(Inc); 
        *++e = PSH; // Push value onto stack
        *++e = IMM; // Load immediate -1
        *++e = -1; 
        *++e = XOR; // Bitwise XOR with -1 (inverts all bits)
        ty = INT; // Result is always an integer
      } 
      
      else if (tk == Add) { // Handle unary plus (e.g., +x) - No effect in computation
        next(); 
        expr(Inc); 
        ty = INT; // Result remains an integer
      } 
      
      else if (tk == Sub) { // Handle unary minus (-x)
        next(); 
        *++e = IMM; // Load immediate value
    
        if (tk == Num) { 
          // If a number follows (-5), simply negate it
          *++e = -ival; 
          next(); 
        } 
        else { 
          // Otherwise, compute (-expr)
          *++e = -1; 
          *++e = PSH; 
          expr(Inc); 
          *++e = MUL; // Multiply expression by -1
        }
    
        ty = INT; // Result is always an integer
      }
      
      else if (tk == Inc || tk == Dec) { // Handle pre-increment (++x) and pre-decrement (--x)
        t = tk; 
        next(); 
        expr(Inc);
    
        // Ensure valid lvalue (must be a variable)
        if (*e == LC) { 
          *e = PSH; 
          *++e = LC; 
        }
        else if (*e == LI) { 
          *e = PSH; 
          *++e = LI; 
        }
        else { 
          printf("%lld: bad lvalue in pre-increment\n", line); 
          exit(-1); 
        }
    
        *++e = PSH; // Push the value before incrementing
        *++e = IMM; // Load the amount to increment/decrement
        *++e = (ty > PTR) ? sizeof(int) : sizeof(char); // Adjust increment size based on type
        *++e = (t == Inc) ? ADD : SUB; // Perform addition or subtraction
        *++e = (ty == CHAR) ? SC : SI; // Store back into memory
      }
      
      else { 
        // Handle invalid expressions
        printf("%lld: bad expression\n", line); 
        exit(-1); 
      }
    
// Function to parse expressions using "precedence climbing"
// This method ensures operations are evaluated in order of precedence,
// processing higher-priority operations (like * and /) before lower-priority ones (like + and -).
// Also known as "Top Down Operator Precedence."
// Precedence climbing is an efficient way to parse expressions without requiring a complex grammar tree.
// Example: Parsing `3 + 2 * 5` should result in `(3 + (2 * 5))`, 
// NOT `((3 + 2) * 5)`, so multiplication gets handled first.

while (tk >= lev) { // "Precedence climbing" method for expression parsing
    t = ty; // Store current expression type for later use

    if (tk == Assign) { // Handle assignment operator (=)
        next();
        if (*e == LC || *e == LI) *e = PSH; // Ensure the left-hand side is a valid lvalue (load from memory)
        else { printf("%lld: bad lvalue in assignment\n", line); exit(-1); } // Error if assignment target is invalid

        expr(Assign); // Parse right-hand side expression
        *++e = ((ty = t) == CHAR) ? SC : SI; // Store result based on type (SC for char, SI for int)
    }

    else if (tk == Cond) { // Handle ternary conditional operator (?:)
        next();
        *++e = BZ; d = ++e; // Emit branch instruction (jump if false)
        expr(Assign); // Parse the first expression (true branch)

        if (tk == ':') next(); // Ensure colon (`:`) is present
        else { printf("%lld: conditional missing colon\n", line); exit(-1); } // Error if missing `:`

        *d = (int)(e + 3); // Set jump destination
        *++e = JMP; d = ++e; // Emit unconditional jump to skip false branch
        expr(Cond); // Parse the false branch
        *d = (int)(e + 1); // Set final jump destination after false branch
    }

    // Logical OR (||)
    // Short-circuit evaluation: If the first operand is true, the result is immediately true.
    else if (tk == Lor) { 
        next();
        *++e = BNZ; d = ++e; // If first operand is true (nonzero), jump past the second operand
        expr(Lan); // Evaluate second operand
        *d = (int)(e + 1); // Set jump target
        ty = INT; // Logical operations result in integer (boolean) values
    } 

    // Logical AND (&&)
    // Short-circuit evaluation: If the first operand is false, the result is immediately false.
    else if (tk == Lan) { 
        next();
        *++e = BZ;  d = ++e; // If first operand is false (zero), jump past the second operand
        expr(Or); // Evaluate second operand
        *d = (int)(e + 1); // Set jump target
        ty = INT; // Logical operations result in integer (boolean) values
    } 

    // Bitwise OR (|) - Computes bitwise OR of two values
    else if (tk == Or)  { 
        next(); 
        *++e = PSH; expr(Xor); // Push first operand and evaluate second operand
        *++e = OR; // Perform bitwise OR operation
        ty = INT;
    } 

    // Bitwise XOR (^) - Computes bitwise XOR of two values
    else if (tk == Xor) { 
        next(); 
        *++e = PSH; expr(And); // Push first operand and evaluate second operand
        *++e = XOR; // Perform bitwise XOR operation
        ty = INT;
    } 

    // Bitwise AND (&) - Computes bitwise AND of two values
    else if (tk == And) { 
        next(); 
        *++e = PSH; expr(Eq); // Push first operand and evaluate second operand
        *++e = AND; // Perform bitwise AND operation
        ty = INT;
    } 

    // Equality comparison (==) - Checks if two values are equal
    else if (tk == Eq)  { 
        next(); 
        *++e = PSH; expr(Lt); // Push first operand and evaluate second operand
        *++e = EQ; // Perform equality comparison
        ty = INT;
    } 

    // Not equal (!=) - Checks if two values are different
    else if (tk == Ne)  { 
        next(); 
        *++e = PSH; expr(Lt); // Push first operand and evaluate second operand
        *++e = NE; // Perform not-equal comparison
        ty = INT;
    } 

    // Less than (<) - Checks if first value is smaller than second
    else if (tk == Lt)  { 
        next(); 
        *++e = PSH; expr(Shl); // Push first operand and evaluate second operand
        *++e = LT; // Perform less-than comparison
        ty = INT;
    } 

    // Greater than (>) - Checks if first value is greater than second
    else if (tk == Gt)  { 
        next(); 
        *++e = PSH; expr(Shl); // Push first operand and evaluate second operand
        *++e = GT; // Perform greater-than comparison
        ty = INT;
    } 

    // Less than or equal (<=) - Checks if first value is smaller or equal to second
    else if (tk == Le)  { 
        next(); 
        *++e = PSH; expr(Shl); // Push first operand and evaluate second operand
        *++e = LE; // Perform less-than-or-equal comparison
        ty = INT;
    } 

    // Greater than or equal (>=) - Checks if first value is greater or equal to second
    else if (tk == Ge)  { 
        next(); 
        *++e = PSH; expr(Shl); // Push first operand and evaluate second operand
        *++e = GE; // Perform greater-than-or-equal comparison
        ty = INT;
    } 

    // Bitwise shift left (<<) - Shifts bits left by specified number of positions
    else if (tk == Shl) { 
        next(); 
        *++e = PSH; expr(Add); // Push first operand and evaluate second operand
        *++e = SHL; // Perform left shift
        ty = INT;
    } 

    // Bitwise shift right (>>) - Shifts bits right by specified number of positions
    else if (tk == Shr) { 
        next(); 
        *++e = PSH; expr(Add); // Push first operand and evaluate second operand
        *++e = SHR; // Perform right shift
        ty = INT;
    } 

    // Addition (+)
    else if (tk == Add) { 
        next(); 
        *++e = PSH; expr(Mul); // Push first operand and evaluate second operand
        if ((ty = t) > PTR) { 
            *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  
        } // Pointer arithmetic scales by sizeof(int)
        *++e = ADD; // Perform addition
    } 

    // Subtraction (-)
    else if (tk == Sub) { 
        next(); 
        *++e = PSH; expr(Mul); // Push first operand and evaluate second operand
        if (t > PTR && t == ty) { 
            *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV; 
            ty = INT;  
        } // Pointer subtraction
        else if ((ty = t) > PTR) { 
            *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB;  
        } // Pointer arithmetic
        else *++e = SUB; // Perform subtraction
    } 

    // Multiplication (*)
    else if (tk == Mul) { 
        next(); 
        *++e = PSH; expr(Inc); // Push first operand and evaluate second operand
        *++e = MUL; // Perform multiplication
        ty = INT;
    } 

    // Division (/)
    else if (tk == Div) { 
        next(); 
        *++e = PSH; expr(Inc); // Push first operand and evaluate second operand
        *++e = DIV; // Perform division
        ty = INT;
    } 

    // Modulo (%)
    else if (tk == Mod) { 
        next(); 
        *++e = PSH; expr(Inc); // Push first operand and evaluate second operand
        *++e = MOD; // Perform modulo operation
        ty = INT;
    } 

    // Post-increment (++) and Post-decrement (--)
    else if (tk == Inc || tk == Dec) { 
        if (*e == LC) { *e = PSH; *++e = LC; } // Load character
        else if (*e == LI) { *e = PSH; *++e = LI; } // Load integer
        else { printf("%lld: bad lvalue in post-increment\n", line); exit(-1); } 

        *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
        *++e = (tk == Inc) ? ADD : SUB; // Increment or decrement
        *++e = (ty == CHAR) ? SC : SI; // Store result back
        *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
        *++e = (tk == Inc) ? SUB : ADD; // Undo effect on returned value
        next();
    } 

    else if (tk == Brak) { // Handle array indexing ([]), e.g., arr[index]
        next(); 
        *++e = PSH; // Push the base address onto the stack
        expr(Assign); // Evaluate the index expression
  
        if (tk == ']') next(); // Ensure the closing bracket is present
        else { printf("%lld: close bracket expected\n", line); exit(-1); } // Error handling for missing closing bracket
  
        if (t > PTR) { // If the base type is a pointer, perform pointer arithmetic
            *++e = PSH; // Push the index value
            *++e = IMM; // Load immediate value
            *++e = sizeof(int); // Get the size of an int (or pointer-sized data)
            *++e = MUL; // Multiply index by element size
        }
        else if (t < PTR) { // Ensure array indexing is only done on pointer types
            printf("%lld: pointer type expected\n", line); exit(-1);
        }
  
        *++e = ADD; // Add the computed offset to the base address
        *++e = ((ty = t - PTR) == CHAR) ? LC : LI; // Load character (LC) if char type, otherwise load integer (LI)
    } else { 
      printf("%lld: compiler error tk=%lld\n", line, tk); 
      exit(-1); // Catch unrecognized tokens or unexpected syntax to prevent compilation errors.
    }
}
// Function to parse and execute statements
// This function handles control flow statements such as `if`, `while`, `return`, blocks `{}`,
// as well as simple expressions and empty statements `;`.
void stmt() {
    int *a, *b; // Pointers used for handling jumps in control flow
  
    if (tk == If) { // Handle `if` statements
      next();
      if (tk == '(') next(); else { printf("%lld: open paren expected\n", line); exit(-1); } // Ensure opening parenthesis
      expr(Assign); // Parse the condition
      if (tk == ')') next(); else { printf("%lld: close paren expected\n", line); exit(-1); } // Ensure closing parenthesis
      *++e = BZ; b = ++e; // Generate branch instruction (jump if false)
      stmt(); // Parse the `if` block
      if (tk == Else) { // Handle optional `else` block
        *b = (int)(e + 3); *++e = JMP; b = ++e; // Jump over `else` if `if` was executed
        next();
        stmt(); // Parse `else` block
      }
      *b = (int)(e + 1); // Set jump destination
    }
  
    else if (tk == While) { // Handle `while` loops
      next();
      a = e + 1; // Store loop start position
      if (tk == '(') next(); else { printf("%lld: open paren expected\n", line); exit(-1); } // Ensure opening parenthesis
      expr(Assign); // Parse loop condition
      if (tk == ')') next(); else { printf("%lld: close paren expected\n", line); exit(-1); } // Ensure closing parenthesis
      *++e = BZ; b = ++e; // Generate branch instruction (exit loop if condition is false)
      stmt(); // Parse loop body
      *++e = JMP; *++e = (int)a; // Jump back to loop start
      *b = (int)(e + 1); // Set exit jump destination
    }
  
    else if (tk == Return) { // Handle `return` statements
      next();
      if (tk != ';') expr(Assign); // If return has an expression, evaluate it
      *++e = LEV; // Emit return instruction
      if (tk == ';') next(); else { printf("%lld: semicolon expected\n", line); exit(-1); } // Ensure semicolon
    }
  
    else if (tk == '{') { // Handle block `{}` statements
      next();
      while (tk != '}') stmt(); // Parse multiple statements inside the block
      next();
    }
  
    else if (tk == ';') { // Handle empty statements
      next();
    }
  
    else { // Handle regular expressions as statements
      expr(Assign); // Evaluate expression
      if (tk == ';') next(); else { printf("%lld: semicolon expected\n", line); exit(-1); } // Ensure semicolon
    }
  }
  

// Main function - Entry point of the program
int main(int argc, char **argv){
  int fd, bt, ty, poolsz, *idmain; // File descriptor, base type, type, memory pool size, main function identifier
  int *pc, *sp, *bp, a, cycle; // Virtual machine (VM) registers
  int i, *t; // Temporary variables

  // Process command-line arguments
  --argc; ++argv;
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; } // Enable source output mode
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; } // Enable debug mode
  if (argc < 1) { 
    printf("usage: c4 [-s] [-d] file ...\n"); 
    return -1; 
  }

  // Open the input source file
  if ((fd = open(*argv, 0)) < 0) { 
    printf("could not open(%s)\n", *argv); 
    return -1; 
  }

  // Allocate memory for different program areas
  poolsz = 256 * 1024; // Set memory pool size (256 KB)
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%lld) symbol area\n", poolsz); return -1; } // Symbol table
  if (!(le = e = malloc(poolsz))) { printf("could not malloc(%lld) text area\n", poolsz); return -1; } // Instruction memory (text segment)
  if (!(data = malloc(poolsz))) { printf("could not malloc(%lld) data area\n", poolsz); return -1; } // Data segment
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%lld) stack area\n", poolsz); return -1; } // Stack memory

  // Initialize allocated memory areas
  memset(sym,  0, poolsz); // Clear symbol table
  memset(e,    0, poolsz); // Clear text segment
  memset(data, 0, poolsz); // Clear data segment

  // Initialize keyword and library function identifiers
  p = "char else enum if int return sizeof while "
      "open read close printf malloc free memset memcmp exit void main";

  // Store keywords in the symbol table
  i = Char; 
  while (i <= While) { 
    next(); // Extract keyword from `p`
    id[Tk] = i++; // Store its token type in symbol table
  } 

  // Store library function names in the symbol table
  i = OPEN; 
  while (i <= EXIT) { 
    next(); 
    id[Class] = Sys;  // Mark as system function
    id[Type] = INT;   // Return type is integer
    id[Val] = i++;    // Store function index
  } 

  next(); 
  id[Tk] = Char; // Handle void type as "char" (C4 does not explicitly support void)

  next(); 
  idmain = id; // Store reference to main function identifier

  // Allocate memory for the source code and read from the file
  if (!(lp = p = malloc(poolsz))) { 
    printf("could not malloc(%lld) source area\n", poolsz); 
    return -1; 
  }
  
  if ((i = read(fd, p, poolsz - 1)) <= 0) { 
    printf("read() returned %lld\n", i); 
    return -1; 
  }

  p[i] = 0; // Null-terminate the source code
  close(fd); // Close the source file

  // Parse declarations (global variables, functions, enums)
  line = 1; // Set initial line number
  next(); // Start parsing
  while (tk) { // Loop through all tokens
    bt = INT; // Default base type is int

    // Handle different base types
    if (tk == Int) next(); 
    else if (tk == Char) { 
      next(); 
      bt = CHAR; // Change base type to char
    } 
    else if (tk == Enum) { // Handle enum declarations
      next();
      if (tk != '{') next(); // Move past enum name if present

      if (tk == '{') { // Handle enum block
        next();
        i = 0; // Enum value counter
        while (tk != '}') { // Parse enum values
          if (tk != Id) { 
            printf("%lld: bad enum identifier %lld\n", line, tk); 
            return -1; 
          }
          next();
          
          if (tk == Assign) { // Handle explicit enum assignments (e.g., ENUM_VAL = 5)
            next();
            if (tk != Num) { 
              printf("%lld: bad enum initializer\n", line); 
              return -1; 
            }
            i = ival; // Assign specified value
            next();
          }

          // Store enum value in symbol table
          id[Class] = Num; 
          id[Type] = INT; 
          id[Val] = i++;

          if (tk == ',') next(); // Handle comma-separated values
        }
        next(); // Move past closing '}'
      }
    }
    }

    // Parse global and function declarations
    while (tk != ';' && tk != '}') { 
        ty = bt; // Set type based on base type (bt)
        
        // Handle pointer types (e.g., int *x, char **y)
        while (tk == Mul) { 
            next(); 
            ty = ty + PTR; // Increment pointer depth
        }
        
        // Ensure identifier follows type declaration
        if (tk != Id) { 
            printf("%lld: bad global declaration\n", line); 
            return -1; 
        }

        // Check for duplicate global definition
        if (id[Class]) { 
            printf("%lld: duplicate global definition\n", line); 
            return -1; 
        }
        next();

        // Store type information
        id[Type] = ty;

        // Function declaration
        if (tk == '(') { 
            id[Class] = Fun; // Mark identifier as function
            id[Val] = (int)(e + 1); // Store function entry point

            next(); 
            i = 0; // Parameter count

            // Parse function parameters
            while (tk != ')') {
                ty = INT; // Default type is int
                
                // Handle parameter type (int, char)
                if (tk == Int) next();
                else if (tk == Char) { 
                    next(); 
                    ty = CHAR; 
                }

                // Handle pointer parameters
                while (tk == Mul) { 
                    next(); 
                    ty = ty + PTR; 
                }

                // Ensure parameter has a valid identifier
                if (tk != Id) { 
                    printf("%lld: bad parameter declaration\n", line); 
                    return -1; 
                }

                // Check for duplicate parameter definition
                if (id[Class] == Loc) { 
                    printf("%lld: duplicate parameter definition\n", line); 
                    return -1; 
                }

                // Store parameter information
                id[HClass] = id[Class]; id[Class] = Loc; // Mark as local variable
                id[HType]  = id[Type];  id[Type] = ty;   // Store type
                id[HVal]   = id[Val];   id[Val] = i++;   // Assign parameter index

                next();
                if (tk == ',') next(); // Handle multiple parameters
            }
            next();

            // Ensure function has a body (curly brace)
            if (tk != '{') { 
                printf("%lld: bad function definition\n", line); 
                return -1; 
            }

            loc = ++i; // Track local variables
            next();

            // Parse local variable declarations
            while (tk == Int || tk == Char) {
                bt = (tk == Int) ? INT : CHAR;
                next();

                // Parse multiple local variables in one statement
                while (tk != ';') {
                    ty = bt;
                    while (tk == Mul) { 
                        next(); 
                        ty = ty + PTR; 
                    }

                    // Ensure valid identifier
                    if (tk != Id) { 
                        printf("%lld: bad local declaration\n", line); 
                        return -1; 
                    }

                    // Check for duplicate local definition
                    if (id[Class] == Loc) { 
                        printf("%lld: duplicate local definition\n", line); 
                        return -1; 
                    }

                    // Store local variable information
                    id[HClass] = id[Class]; id[Class] = Loc;
                    id[HType]  = id[Type];  id[Type] = ty;
                    id[HVal]   = id[Val];   id[Val] = ++i;

                    next();
                    if (tk == ',') next(); // Handle multiple local variables
                }
                next();
            }

            // Generate function prologue
            *++e = ENT; 
            *++e = i - loc; // Allocate space for local variables

            // Parse function body
            while (tk != '}') stmt();

            // Generate function epilogue
            *++e = LEV;

            // Restore symbol table for local variables
            id = sym; 
            while (id[Tk]) {
                if (id[Class] == Loc) {
                    id[Class] = id[HClass]; // Restore previous class
                    id[Type] = id[HType];   // Restore previous type
                    id[Val] = id[HVal];     // Restore previous value
                }
                id = id + Idsz; // Move to next identifier
            }
        }
        else { // Global variable declaration
            id[Class] = Glo; // Mark as global variable
            id[Val] = (int)data; // Assign address in data section
            data = data + sizeof(int); // Reserve memory space
        }

        if (tk == ',') next(); // Handle multiple declarations
    }
    next();

  // Ensure that the main function is defined in the program
    if (!(pc = (int *)idmain[Val])) { 
        printf("main() not defined\n"); 
        return -1; 
    }

    // If source mode is enabled, exit before execution
    if (src) return 0;

    // Setup the stack
    bp = sp = (int *)((int)sp + poolsz); // Initialize stack pointer
    *--sp = EXIT; // Ensure program calls exit if main() returns
    *--sp = PSH; t = sp; // Store stack pointer for function call setup
    *--sp = argc; // Push argument count
    *--sp = (int)argv; // Push program arguments
    *--sp = (int)t; // Push stack pointer backup

    // Start execution cycle
    cycle = 0;
    while (1) {
        i = *pc++; // Fetch instruction
        ++cycle; // Count number of executed instructions
        
        // Debug mode: Print executed instruction
        if (debug) {
            printf("%lld> %.4s", cycle,
                &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[i * 5]);
            if (i <= ADJ) printf(" %lld\n", *pc); else printf("\n");
        }

        // Execute instructions based on opcode
        if      (i == LEA) a = (int)(bp + *pc++);  // Load local address
        else if (i == IMM) a = *pc++;              // Load immediate value
        else if (i == JMP) pc = (int *)*pc;        // Unconditional jump
        else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; } // Jump to subroutine
        else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc; // Branch if zero
        else if (i == BNZ) pc = a ? (int *)*pc : pc + 1; // Branch if not zero
        else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; } // Enter subroutine
        else if (i == ADJ) sp = sp + *pc++; // Adjust stack
        else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // Leave subroutine
        else if (i == LI)  a = *(int *)a; // Load integer
        else if (i == LC)  a = *(char *)a; // Load character
        else if (i == SI)  *(int *)*sp++ = a; // Store integer
        else if (i == SC)  a = *(char *)*sp++ = a; // Store character
        else if (i == PSH) *--sp = a; // Push value onto stack

        // Perform arithmetic and logical operations
        else if (i == OR)  a = *sp++ |  a;
        else if (i == XOR) a = *sp++ ^  a;
        else if (i == AND) a = *sp++ &  a;
        else if (i == EQ)  a = *sp++ == a;
        else if (i == NE)  a = *sp++ != a;
        else if (i == LT)  a = *sp++ <  a;
        else if (i == GT)  a = *sp++ >  a;
        else if (i == LE)  a = *sp++ <= a;
        else if (i == GE)  a = *sp++ >= a;
        else if (i == SHL) a = *sp++ << a;
        else if (i == SHR) a = *sp++ >> a;
        else if (i == ADD) a = *sp++ +  a;
        else if (i == SUB) a = *sp++ -  a;
        else if (i == MUL) a = *sp++ *  a;
        else if (i == DIV) a = *sp++ /  a;
        else if (i == MOD) a = *sp++ %  a;

        // Handle system calls
        else if (i == OPEN) a = open((char *)sp[1], *sp); // Open file
        else if (i == READ) a = read(sp[2], (char *)sp[1], *sp); // Read file
        else if (i == CLOS) a = close(*sp); // Close file
        else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); } // Print formatted output
        else if (i == MALC) a = (int)malloc(*sp); // Allocate memory
        else if (i == FREE) free((void *)*sp); // Free allocated memory
        else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp); // Set memory
        else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp); // Compare memory
        else if (i == EXIT) { 
            printf("exit(%lld) cycle = %lld\n", *sp, cycle); 
            return *sp; // Exit program
        }
        else { 
            printf("unknown instruction = %lld! cycle = %lld\n", i, cycle); 
            return -1; // Handle unknown instruction error
        }
    }
    }
    }
   
}
