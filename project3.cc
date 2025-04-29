#include <cstdlib>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <map>
#include <vector> // Needed for vector
#include <string> // Needed for stoi, string comparison
#include <iostream>

// Assuming execute.h defines InstructionNode, mem, next_available, inputs etc.
// AND defines enum ConditionalOperatorType { CONDITION_GREATER, CONDITION_LESS, CONDITION_NOTEQUAL, ... };
#include "execute.h"
// Assuming lexer.h defines LexicalAnalyzer, Token, TokenType { ID, COMMA, SEMICOLON, LBRACE, RBRACE, NUM, INPUT, OUTPUT, WHILE, IF, SWITCH, FOR, EQUAL, PLUS, MINUS, MULT, DIV, GREATER, LESS, NOTEQUAL, LPAREN, RPAREN, CASE, DEFAULT, COLON, END_OF_FILE, ... } etc.
// Assuming Token has a Print() method.
#include "lexer.h"

// Global lexer instance
LexicalAnalyzer lexer;
// Map variable names to memory locations
std::map<std::string, int> location_map;
// Assume these are defined in execute.h or elsewhere globally
extern int mem[1000];
extern int next_available;
extern std::vector<int> inputs;

// --- Helper Struct for Parsing Results ---
// Used to return both the start and end nodes of a parsed code block,
// crucial for correctly linking control flow structures.
struct ParseResult {
    InstructionNode* head = nullptr;      // First instruction of the parsed block
    InstructionNode* exit_node = nullptr; // Node where execution continues AFTER this block
                                          // For simple statements, exit_node == head.
                                          // For control flow, it's usually the final NOOP.
};

// --- Forward Declarations ---
ParseResult parse_program();
void parse_var_section();
ParseResult parse_body();
void parse_inputs();
ParseResult parse_stmt_list();
ParseResult parse_stmt();
ParseResult parse_assign_stmt();
ParseResult parse_input_stmt();
ParseResult parse_output_stmt();
ParseResult parse_while_stmt();
ParseResult parse_if_stmt();
ParseResult parse_switch_stmt();
ParseResult parse_for_stmt();
// Fills CJMP fields based on parsed condition.
void parse_condition(InstructionNode* inst);

// --- Function Implementations ---

// Utility to create a NOOP node. Used as targets for jumps and exits.
InstructionNode* create_noop_node() {
    InstructionNode* noop = new InstructionNode();
    noop->type = NOOP;
    noop->next = nullptr;
    // Ensure any union members are initialized if necessary
    // noop->jmp_inst.target = nullptr; // Example if NOOP used union
    return noop;
}


// Main parsing function (entry point called by external code)
struct InstructionNode * parse_Generate_Intermediate_Representation()
{
    // Reset global state if necessary before parsing
    location_map.clear();
    // next_available = 0; // Reset memory counter if needed by execution setup

    // Start parsing the program structure
    ParseResult program_result = parse_program();

    // Return the head of the generated instruction list
    return program_result.head;
}

// Parses: program -> var_section body inputs
ParseResult parse_program() {
    // 1. Parse variable declarations
    parse_var_section();
    // 2. Parse the main body of the program
    ParseResult body_result = parse_body();
    // 3. Parse the input values provided after the code
    parse_inputs();

    // The 'exit' of the program isn't a specific node in the list,
    // execution just stops after the last instruction from the body.
    // Return the result containing the head of the body's instructions.
    return body_result;
}

// Parses: var_section -> id_list SEMICOLON
// id_list -> ID | ID COMMA id_list
void parse_var_section() {
    Token t = lexer.GetToken(); // Expect first ID
    if (t.token_type != ID) {
        // Removed token_to_string
        std::cerr << "Syntax Error (var_section): Expected ID, found token with lexeme '" << t.lexeme << "' at line " << t.line_no << "\n";
        exit(1);
    }
    // Check for redeclaration
    if (location_map.count(t.lexeme)) {
         std::cerr << "Error (var_section): Variable '" << t.lexeme << "' declared multiple times at line " << t.line_no << "\n";
         exit(1);
    }
    // Assign the next available memory location to this variable
    location_map[t.lexeme] = next_available++;

    // Consume subsequent ID declarations separated by COMMA
    while (lexer.peek(1).token_type == COMMA) {
        lexer.GetToken(); // consume COMMA
        t = lexer.GetToken(); // Expect next ID
        if (t.token_type != ID) {
            // Removed token_to_string
            std::cerr << "Syntax Error (var_section): Expected ID after COMMA, found token with lexeme '" << t.lexeme << "' at line " << t.line_no << "\n";
            exit(1);
        }
        if (location_map.count(t.lexeme)) {
             std::cerr << "Error (var_section): Variable '" << t.lexeme << "' declared multiple times at line " << t.line_no << "\n";
             exit(1);
        }
        location_map[t.lexeme] = next_available++;
    }

    // Expect SEMICOLON to terminate the variable section
    t = lexer.GetToken();
    if (t.token_type != SEMICOLON) {
        // Removed token_to_string
        std::cerr << "Syntax Error (var_section): Expected SEMICOLON, found token with lexeme '" << t.lexeme << "' at line " << t.line_no << "\n";
        exit(1);
    }
}

// Parses: body -> LBRACE stmt_list RBRACE
ParseResult parse_body() {
    Token t = lexer.GetToken(); // Expect LBRACE
    if (t.token_type != LBRACE) {
        // Removed token_to_string
        std::cerr << "Syntax Error (parse_body): Expected LBRACE '{', found token with lexeme '" << t.lexeme << "' at line " << t.line_no << "\n";
        exit(1);
    }

    // Handle empty body: {}
    if (lexer.peek(1).token_type == RBRACE) {
        lexer.GetToken(); // Consume RBRACE
        InstructionNode* noop = create_noop_node();
        // An empty body is represented by a single NOOP instruction.
        return {noop, noop};
    }

    // If not empty, parse the list of statements within the braces
    ParseResult body_result = parse_stmt_list();

    // Expect RBRACE to close the body
    t = lexer.GetToken();
    if (t.token_type != RBRACE) {
        // Removed token_to_string
        std::cerr << "Syntax Error (parse_body): Expected RBRACE '}' to close body, found token with lexeme '" << t.lexeme << "' at line " << t.line_no << "\n";
        exit(1);
    }

    // Return the result (head and exit node) from parsing the statement list
    return body_result;
}

// Parses: inputs -> num_list
// num_list -> NUM | NUM num_list
void parse_inputs() {
    inputs.clear(); // Clear any previous inputs
    Token t = lexer.GetToken(); // Expect first NUM
    // Check if the input section is mandatory or can be empty.
    // Assuming at least one number is required based on original code structure.
    if (t.token_type != NUM) {
        // Removed token_to_string
        std::cerr << "Syntax Error (parse_inputs): Expected NUM, found token with lexeme '" << t.lexeme << "' at line " << t.line_no << "\n";
        exit(1);
    }
    // Convert lexeme to integer and store it
    inputs.push_back(std::stoi(t.lexeme));

    // Consume subsequent NUM tokens
    while (lexer.peek(1).token_type == NUM) {
        t = lexer.GetToken();
        inputs.push_back(std::stoi(t.lexeme));
    }

    // After the numbers, we should be at the end of the input file.
    Token peek_eof = lexer.peek(1); // Peek for EOF
    if (peek_eof.token_type != END_OF_FILE) {
         // Removed token_to_string
         std::cerr << "Syntax Warning (parse_inputs): Expected END_OF_FILE after inputs, found token with lexeme '" << peek_eof.lexeme << "' at line " << peek_eof.line_no << "\n";
         // Depending on requirements, this could be an error: exit(1);
    }
}


// Parses: stmt_list -> stmt stmt_list | stmt
// Links statements sequentially using the ParseResult exit nodes.
ParseResult parse_stmt_list() {
    // Parse the first statement
    ParseResult first_result = parse_stmt();
    InstructionNode* head = first_result.head;
    InstructionNode* current_exit = first_result.exit_node;

    // Basic check: ensure the first statement parsing was successful.
    if (head == nullptr || current_exit == nullptr) {
         std::cerr << "Internal Error (parse_stmt_list): Null result from first parse_stmt.\n";
         exit(1); // Critical internal error
    }

    // Loop while the next token indicates the start of another statement
    while (true) {
        Token next_token = lexer.peek(1);

        // Check if the next token can start a statement
        if (next_token.token_type == ID || next_token.token_type == WHILE || next_token.token_type == IF ||
            next_token.token_type == SWITCH || next_token.token_type == FOR ||
            next_token.token_type == INPUT || next_token.token_type == OUTPUT)
        {
            // Parse the next statement
            ParseResult next_result = parse_stmt();

            // Check for parsing errors in the subsequent statement
            if (next_result.head == nullptr || next_result.exit_node == nullptr) {
                 std::cerr << "Internal Error (parse_stmt_list): Null result from subsequent parse_stmt.\n";
                 exit(1); // Critical internal error
            }

            // *** CRITICAL LINKING STEP ***
            // Link the exit point of the *previous* statement/block
            // to the head (entry point) of the *current* statement/block.
            current_exit->next = next_result.head;

            // Update the overall exit node for the *entire list* processed so far.
            // It becomes the exit node of the *most recently parsed* statement.
            current_exit = next_result.exit_node;

        } else {
            // The next token doesn't start a statement, so we've reached the end
            // of the statement list (e.g., encountered RBRACE).
            break;
        }
    }

    // Return the head of the first statement and the exit node of the last statement.
    return {head, current_exit};
}


// Parses: stmt -> assign_stmt | input_stmt | output_stmt | while_stmt | if_stmt | switch_stmt | for_stmt
// Determines the type of statement based on the first token and calls the appropriate parser.
ParseResult parse_stmt() {
    Token t = lexer.peek(1); // Look ahead to see what kind of statement it is
    switch (t.token_type) {
        case ID:     return parse_assign_stmt();
        case INPUT:  return parse_input_stmt();
        case OUTPUT: return parse_output_stmt();
        case WHILE:  return parse_while_stmt();
        case IF:     return parse_if_stmt();
        case SWITCH: return parse_switch_stmt();
        case FOR:    return parse_for_stmt();
        default:
            // If the token doesn't match any known statement start
            // Removed token_to_string
            std::cerr << "Syntax Error (parse_stmt): Unexpected token with lexeme '" << t.lexeme << "' found when expecting start of a statement at line " << t.line_no << "\n";
            exit(1);
            // Should not be reached due to exit(1)
            // return {nullptr, nullptr};
    }
}

// Parses: assign_stmt -> ID EQUAL primary (operator primary)? SEMICOLON
// primary -> ID | NUM
// operator -> PLUS | MINUS | MULT | DIV
ParseResult parse_assign_stmt() {
    Token lhs = lexer.GetToken(); // consume ID (already peeked by parse_stmt)
    // Check if variable exists (was declared)
    if (!location_map.count(lhs.lexeme)) {
        std::cerr << "Error (parse_assign_stmt): Variable '" << lhs.lexeme << "' not declared before use at line " << lhs.line_no << "\n";
        exit(1);
    }

    Token eq = lexer.GetToken(); // consume EQUAL
    if (eq.token_type != EQUAL) {
        // Removed token_to_string
        std::cerr << "Syntax Error (parse_assign_stmt): Expected '=', found token with lexeme '" << eq.lexeme << "' after variable '" << lhs.lexeme << "' at line " << eq.line_no << "\n";
        exit(1);
    }

    // Create the instruction node for assignment
    InstructionNode* inst = new InstructionNode();
    inst->type = ASSIGN;
    inst->assign_inst.lhs_loc = location_map[lhs.lexeme]; // Store location of variable being assigned to
    inst->next = nullptr; // Will be linked later by parse_stmt_list

    // Parse the right-hand side (RHS)
    // Parse first operand (primary)
    Token first = lexer.GetToken(); // Expect ID or NUM
    int op1_loc = -1; // Initialize location index

    if (first.token_type == ID) {
        // If operand is a variable, check if it's declared
        if (!location_map.count(first.lexeme)) {
             std::cerr << "Error (parse_assign_stmt): Variable '" << first.lexeme << "' used on RHS not declared at line " << first.line_no << "\n";
             exit(1);
        }
        op1_loc = location_map[first.lexeme]; // Get location of the variable
    } else if (first.token_type == NUM) {
        // If operand is a number, store it in the next available memory slot (as a constant)
        mem[next_available] = std::stoi(first.lexeme);
        op1_loc = next_available++; // Use the location of this new constant
    } else {
        // If not ID or NUM, it's a syntax error
        // Removed token_to_string
        std::cerr << "Syntax Error (parse_assign_stmt): Expected ID or NUM for first operand on RHS, found token with lexeme '" << first.lexeme << "' at line " << first.line_no << "\n";
        exit(1);
    }

    // Check if there is an operator (PLUS, MINUS, MULT, DIV) following the first operand
    Token op_token = lexer.peek(1);
    if (op_token.token_type == PLUS || op_token.token_type == MINUS ||
        op_token.token_type == MULT || op_token.token_type == DIV) {

        lexer.GetToken(); // Consume the operator token

        inst->assign_inst.op1_loc = op1_loc; // Store location of the first operand

        // Parse second operand (primary)
        Token second = lexer.GetToken(); // Expect ID or NUM
        int op2_loc = -1; // Initialize location index

        if (second.token_type == ID) {
            // Check if variable is declared
             if (!location_map.count(second.lexeme)) {
                 std::cerr << "Error (parse_assign_stmt): Variable '" << second.lexeme << "' used as second operand not declared at line " << second.line_no << "\n";
                 exit(1);
             }
            op2_loc = location_map[second.lexeme]; // Get location
        } else if (second.token_type == NUM) {
            // Store number constant in memory
            mem[next_available] = std::stoi(second.lexeme);
            op2_loc = next_available++; // Use its location
        } else {
            // Syntax error if not ID or NUM
            // Removed token_to_string
            std::cerr << "Syntax Error (parse_assign_stmt): Expected ID or NUM for second operand, found token with lexeme '" << second.lexeme << "' at line " << second.line_no << "\n";
            exit(1);
        }

        inst->assign_inst.op2_loc = op2_loc; // Store location of the second operand

        // Set the operator type in the instruction node based on the token
        switch (op_token.token_type) {
            case PLUS:  inst->assign_inst.op = OPERATOR_PLUS; break;
            case MINUS: inst->assign_inst.op = OPERATOR_MINUS; break;
            case MULT:  inst->assign_inst.op = OPERATOR_MULT; break;
            case DIV:   inst->assign_inst.op = OPERATOR_DIV; break;
            default: /* Should not happen due to peek check */ break;
        }
    } else {
        // No operator found - this is a simple assignment (e.g., x = y; or x = 5;)
        inst->assign_inst.op = OPERATOR_NONE; // Indicate no arithmetic operation
        inst->assign_inst.op1_loc = op1_loc; // The value being assigned is the first operand
        inst->assign_inst.op2_loc = -1; // Use -1 or another indicator for no second operand
    }

    // Expect SEMICOLON to terminate the assignment statement
    Token semi = lexer.GetToken();
    if (semi.token_type != SEMICOLON) {
        // Removed token_to_string
        std::cerr << "Syntax Error (parse_assign_stmt): Expected ';' after assignment statement, found token with lexeme '" << semi.lexeme << "' at line " << semi.line_no << "\n";
        exit(1);
    }

    // For a simple assignment, the instruction itself is both the head and the exit point.
    return {inst, inst};
}


// Parses: input_stmt -> INPUT ID SEMICOLON
ParseResult parse_input_stmt() {
    lexer.GetToken(); // consume INPUT (already peeked by parse_stmt)

    Token var = lexer.GetToken(); // Expect ID
    if (var.token_type != ID) {
        // Removed token_to_string
        std::cerr << "Syntax Error (parse_input_stmt): Expected ID after INPUT keyword, found token with lexeme '" << var.lexeme << "' at line " << var.line_no << "\n";
        exit(1);
    }
    // Check if the variable to store the input into has been declared
     if (!location_map.count(var.lexeme)) {
        std::cerr << "Error (parse_input_stmt): Variable '" << var.lexeme << "' not declared before use in input statement at line " << var.line_no << "\n";
        exit(1);
    }

    Token semi = lexer.GetToken(); // Expect SEMICOLON
    if (semi.token_type != SEMICOLON) {
        // Removed token_to_string
        std::cerr << "Syntax Error (parse_input_stmt): Expected ';' after input statement, found token with lexeme '" << semi.lexeme << "' at line " << semi.line_no << "\n";
        exit(1);
    }

    // Create the instruction node for input
    InstructionNode* inst = new InstructionNode();
    inst->type = IN;
    // Store the memory location where the input value should be placed
    inst->input_inst.var_loc = location_map[var.lexeme];
    inst->next = nullptr; // Will be linked later

    // Input instruction is its own head and exit.
    return {inst, inst};
}

// Parses: output_stmt -> OUTPUT ID SEMICOLON
ParseResult parse_output_stmt() {
    lexer.GetToken(); // consume OUTPUT (already peeked by parse_stmt)

    Token var = lexer.GetToken(); // Expect ID
    if (var.token_type != ID) {
        // Removed token_to_string
        std::cerr << "Syntax Error (parse_output_stmt): Expected ID after OUTPUT keyword, found token with lexeme '" << var.lexeme << "' at line " << var.line_no << "\n";
        exit(1);
    }
    // Check if the variable to be outputted has been declared
     if (!location_map.count(var.lexeme)) {
        std::cerr << "Error (parse_output_stmt): Variable '" << var.lexeme << "' not declared before use in output statement at line " << var.line_no << "\n";
        exit(1);
    }

    Token semi = lexer.GetToken(); // Expect SEMICOLON
    if (semi.token_type != SEMICOLON) {
        // Removed token_to_string
        std::cerr << "Syntax Error (parse_output_stmt): Expected ';' after output statement, found token with lexeme '" << semi.lexeme << "' at line " << semi.line_no << "\n";
        exit(1);
    }

    // Create the instruction node for output
    InstructionNode* inst = new InstructionNode();
    inst->type = OUT;
    // Store the memory location of the variable whose value should be outputted
    inst->output_inst.var_loc = location_map[var.lexeme];
    inst->next = nullptr; // Will be linked later

    // Output instruction is its own head and exit.
    return {inst, inst};
}


// Parses: while_stmt -> WHILE condition body
ParseResult parse_while_stmt() {
    lexer.GetToken(); // consume WHILE

    // Create the conditional jump node (CJMP) for the loop condition
    InstructionNode* cond_node = new InstructionNode();
    cond_node->type = CJMP;
    // Parse the condition (e.g., a < 10). This fills the operands and operator in cond_node.
    // `parse_condition` sets the operator directly (e.g., CONDITION_LESS).
    // The execution logic for CJMP must handle jumping based on this condition.
    // Typically, CJMP jumps to its target if the condition is FALSE.
    parse_condition(cond_node);

    // Parse the loop body (statements within {})
    ParseResult body_result = parse_body();

    // Create an unconditional jump (JMP) node to loop back to the condition check
    InstructionNode* jump_back = new InstructionNode();
    jump_back->type = JMP;
    jump_back->jmp_inst.target = cond_node; // Target is the condition node itself
    jump_back->next = nullptr; // This JMP is the last instruction in the loop's body sequence

    // Create a NOOP node to serve as the exit point of the while loop
    InstructionNode* exit_noop = create_noop_node();

    // --- Linking the components ---
    // 1. If condition is TRUE (CJMP doesn't jump), execute the body.
    cond_node->next = body_result.head;
    // 2. If condition is FALSE (CJMP jumps), exit the loop.
    cond_node->cjmp_inst.target = exit_noop;
    // 3. After the body finishes, jump back to the condition check.
    body_result.exit_node->next = jump_back;
    // jump_back->next is implicitly null here, execution flow is handled by the jump.

    // Return the condition node as the head and the NOOP as the exit point.
    return {cond_node, exit_noop};
}


// Parses: if_stmt -> IF condition body
ParseResult parse_if_stmt() {
    lexer.GetToken(); // consume IF

    // Create the conditional jump node (CJMP) for the if condition
    InstructionNode* cond_node = new InstructionNode();
    cond_node->type = CJMP;
    // Parse the condition. Assumes CJMP jumps to target if condition is FALSE.
    parse_condition(cond_node);

    // Parse the body of the if statement (statements within {})
    ParseResult body_result = parse_body();

    // Create a NOOP node to serve as the common exit point after the if statement
    InstructionNode* exit_noop = create_noop_node();

    // --- Linking the components ---
    // 1. If condition is TRUE (CJMP doesn't jump), execute the body.
    cond_node->next = body_result.head;
    // 2. If condition is FALSE (CJMP jumps), skip the body and go to the exit.
    cond_node->cjmp_inst.target = exit_noop;
    // 3. After the body finishes, continue to the exit point.
    body_result.exit_node->next = exit_noop;

    // Return the condition node as the head and the NOOP as the exit point.
    return {cond_node, exit_noop};
}


// Parses: switch_stmt -> SWITCH ID LBRACE case_list (default_case)? RBRACE
//         case_list -> case case_list | case
//         case -> CASE NUM COLON body
//         default_case -> DEFAULT COLON body
ParseResult parse_switch_stmt() {
    lexer.GetToken(); // consume SWITCH

    // --- Get Switch Variable ---
    Token var_token = lexer.GetToken(); // Expect ID
    if (var_token.token_type != ID) {
        std::cerr << "Syntax Error (parse_switch_stmt): Expected variable ID after SWITCH, found token with lexeme '" << var_token.lexeme << "' at line " << var_token.line_no << "\n";
        exit(1);
    }
    if (!location_map.count(var_token.lexeme)) {
        std::cerr << "Error (parse_switch_stmt): Switch variable '" << var_token.lexeme << "' not declared at line " << var_token.line_no << "\n";
        exit(1);
    }
    int switch_var_loc = location_map[var_token.lexeme];

    // --- Consume LBRACE ---
    Token brace = lexer.GetToken(); // Expect LBRACE
    if (brace.token_type != LBRACE) {
        std::cerr << "Syntax Error (parse_switch_stmt): Expected '{' after switch variable, found token with lexeme '" << brace.lexeme << "' at line " << brace.line_no << "\n";
        exit(1);
    }

    // --- Data Structures for Linking ---
    std::vector<InstructionNode*> case_cjmp_nodes; // Stores the CJMP for each case
    std::vector<ParseResult> case_body_results;   // Stores the parsed body for each case
    ParseResult default_body_result = {nullptr, nullptr}; // Stores the parsed default body
    InstructionNode* head_node = nullptr; // The entry point to the entire switch block
    InstructionNode* last_code_exit = nullptr; // Tracks the exit of the previous code block (case/default) for fall-through linking

    // --- Create Overall Exit Node ---
    InstructionNode* final_exit_noop = create_noop_node();

    // --- Parse Case List ---
    while (lexer.peek(1).token_type == CASE) {
        lexer.GetToken(); // consume CASE

        Token num_token = lexer.GetToken(); // Expect NUM
        if (num_token.token_type != NUM) {
            std::cerr << "Syntax Error (parse_switch_stmt): Expected number (NUM) after CASE, found token with lexeme '" << num_token.lexeme << "' at line " << num_token.line_no << "\n";
            exit(1);
        }
        // Store case value as a constant in memory
        mem[next_available] = std::stoi(num_token.lexeme);
        int case_val_loc = next_available++;

        Token colon_token = lexer.GetToken(); // Expect COLON
        if (colon_token.token_type != COLON) {
            std::cerr << "Syntax Error (parse_switch_stmt): Expected ':' after case value, found token with lexeme '" << colon_token.lexeme << "' at line " << colon_token.line_no << "\n";
            exit(1);
        }

        // Create CJMP node for this case comparison (switch_var != case_val)
        InstructionNode* cjmp_node = new InstructionNode();
        cjmp_node->type = CJMP;
        cjmp_node->cjmp_inst.op1_loc = switch_var_loc;
        cjmp_node->cjmp_inst.op2_loc = case_val_loc;
        cjmp_node->cjmp_inst.condition_op = CONDITION_NOTEQUAL; // Jump if NOT equal
        // cjmp_node->cjmp_inst.target will be linked later (to next check or default or final exit)
        cjmp_node->next = nullptr; // Will point to the case body

        case_cjmp_nodes.push_back(cjmp_node);

        // --- Link previous check/body to this CJMP ---
        if (last_code_exit != nullptr) {
             // This handles fall-through *between* cases IF the previous block was code
             // For the very first case, the 'previous' is the start, linked later
             last_code_exit->next = cjmp_node;
        } else if (!case_cjmp_nodes.empty() && case_cjmp_nodes.size() > 1) {
             // Link the previous CJMP's target (if it existed and wasn't the first)
             // This case is subtle: if previous case body was empty {}, its exit == its head (the CJMP)
             // This logic needs careful review depending on empty body handling.
             // Let's simplify: The main linking happens *after* parsing all bodies.
        }


        if (head_node == nullptr) {
            head_node = cjmp_node; // First CJMP is the entry point
        }


        // Parse the body for this case
        ParseResult body_res = parse_body();
        if (body_res.head == nullptr || body_res.exit_node == nullptr) {
             std::cerr << "Internal Error (parse_switch_stmt): Failed to parse body for case " << num_token.lexeme << " at line " << num_token.line_no << "\n";
             exit(1);
        }
        case_body_results.push_back(body_res);

        // Link the CJMP's fall-through (if condition is false) to the body head
        cjmp_node->next = body_res.head;

        // Update the exit tracker for the next fall-through link
        last_code_exit = body_res.exit_node;

    } // End while(CASE)

    // --- Parse Default Case (Optional) ---
    if (lexer.peek(1).token_type == DEFAULT) {
        lexer.GetToken(); // consume DEFAULT

        Token colon_token = lexer.GetToken(); // Expect COLON
        if (colon_token.token_type != COLON) {
            std::cerr << "Syntax Error (parse_switch_stmt): Expected ':' after DEFAULT, found token with lexeme '" << colon_token.lexeme << "' at line " << colon_token.line_no << "\n";
            exit(1);
        }

        // Parse the default body
        default_body_result = parse_body();
         if (default_body_result.head == nullptr || default_body_result.exit_node == nullptr) {
             std::cerr << "Internal Error (parse_switch_stmt): Failed to parse body for default case at line " << colon_token.line_no << "\n";
             exit(1);
        }


        // --- Link previous check/body to the default body ---
        if (last_code_exit != nullptr) {
             // Fall-through from last case body OR jump from last CJMP failure
             last_code_exit->next = default_body_result.head;
        } else if (!case_cjmp_nodes.empty()) {
            // If there were cases, the last CJMP failure should jump here
             // This linking happens in the next phase.
        } else {
            // No cases, only default. Default body is the head.
            head_node = default_body_result.head;
        }

        // Update the exit tracker
        last_code_exit = default_body_result.exit_node;
    }

    // --- Consume RBRACE ---
    brace = lexer.GetToken(); // Expect RBRACE
    if (brace.token_type != RBRACE) {
        std::cerr << "Syntax Error (parse_switch_stmt): Expected '}' to close switch statement, found token with lexeme '" << brace.lexeme << "' at line " << brace.line_no << "\n";
        exit(1);
    }

    // --- Linking Phase ---

    InstructionNode* next_check_target = (default_body_result.head != nullptr) ? default_body_result.head : final_exit_noop;

    // Link CJMP targets (for when switch_var != case_val)
    // Iterate backwards to easily find the 'next' target
    for (int i = case_cjmp_nodes.size() - 1; i >= 0; --i) {
        case_cjmp_nodes[i]->cjmp_inst.target = next_check_target;
        // The target for the *next* (previous in loop) CJMP is this CJMP node
        next_check_target = case_cjmp_nodes[i];
    }

    // Link fall-through from bodies
    // Handled partially during parsing by updating last_code_exit->next
    // Final link from the very last block to the exit_noop:
    if (last_code_exit != nullptr) { // If there was any code (case or default)
         last_code_exit->next = final_exit_noop;
    } else if (head_node != nullptr) { // Cases existed, but all had empty bodies?
        // This implies the last CJMP failure should go directly to exit_noop if no default.
        // This was handled in the CJMP linking loop.
    }
    else {
        // No cases, no default. The switch is effectively empty.
        head_node = final_exit_noop; // Head is just the exit node
    }


    // Handle the case of an empty switch (no cases, no default)
    if (head_node == nullptr) {
        head_node = final_exit_noop;
    }


    // Return the overall head and the final exit node
    return {head_node, final_exit_noop};
}


// Parses: for_stmt -> FOR LPAREN assign_stmt condition SEMICOLON assign_stmt RPAREN body
// Assumes C-style for loop structure: for (initializer; condition; update) { body }
ParseResult parse_for_stmt() {
    lexer.GetToken(); // consume FOR
    Token t = lexer.GetToken(); // consume LPAREN
    if (t.token_type != LPAREN) {
        // Removed token_to_string
        std::cerr << "Syntax Error (parse_for_stmt): Expected '(' after FOR, found token with lexeme '" << t.lexeme << "' at line " << t.line_no << "\n";
        exit(1);
    }

    // 1. Parse Initializer Assignment (e.g., i = 0;)
    // parse_assign_stmt consumes the semicolon.
    ParseResult init_assign_res = parse_assign_stmt();

    // 2. Parse Condition (e.g., i < 10)
    // Create the CJMP node for the loop condition.
    InstructionNode* cond_node = new InstructionNode();
    cond_node->type = CJMP;
    // Parse the condition expression, filling the CJMP node.
    // Assumes CJMP jumps to target if condition is FALSE.
    parse_condition(cond_node); // Consumes primary relop primary

    // Consume the SEMICOLON after the condition
    t = lexer.GetToken();
    if (t.token_type != SEMICOLON) {
        // Removed token_to_string
        std::cerr << "Syntax Error (parse_for_stmt): Expected ';' after condition, found token with lexeme '" << t.lexeme << "' at line " << t.line_no << "\n";
        exit(1);
    }

    // 3. Parse Update Assignment (e.g., i = i + 1;)
    // parse_assign_stmt consumes the semicolon.
    ParseResult update_assign_res = parse_assign_stmt();

    // Consume the closing RPAREN ')'
    t = lexer.GetToken();
    if (t.token_type != RPAREN) {
        // Removed token_to_string
        std::cerr << "Syntax Error (parse_for_stmt): Expected ')' after update statement, found token with lexeme '" << t.lexeme << "' at line " << t.line_no << "\n";
        exit(1);
    }

    // 4. Parse the Loop Body
    ParseResult body_res = parse_body();

    // Create the exit NOOP node for the entire for loop
    InstructionNode* exit_noop = create_noop_node();

    // --- Linking the components ---
    // a. Initializer runs once, then goes to the first condition check.
    init_assign_res.exit_node->next = cond_node;

    // b. If condition is TRUE (CJMP doesn't jump), execute the body.
    cond_node->next = body_res.head;

    // c. If condition is FALSE (CJMP jumps), exit the loop.
    cond_node->cjmp_inst.target = exit_noop;

    // d. After the body finishes, execute the update statement.
    body_res.exit_node->next = update_assign_res.head;

    // e. After the update statement finishes, jump back to the condition check.
    update_assign_res.exit_node->next = cond_node;

    // Return the head of the initializer as the entry point and the NOOP as the exit point.
    return {init_assign_res.head, exit_noop};
}


// Parses: condition -> primary relop primary
// primary -> ID | NUM
// relop -> GREATER | LESS | NOTEQUAL
// Fills the fields (op1_loc, op2_loc, condition_op) of the provided CJMP instruction node.
// *** IMPORTANT: This version directly maps the parsed operator (>, <, !=)
//     to the corresponding enum value (CONDITION_GREATER, CONDITION_LESS, CONDITION_NOTEQUAL).
//     It assumes the execution logic for CJMP knows how to handle these direct conditions
//     (e.g., whether CJMP target is taken on TRUE or FALSE depends on execute()).
void parse_condition(InstructionNode* inst) {
    // --- Parse First Operand (primary) ---
    Token left = lexer.GetToken(); // Expect ID or NUM
    int op1_loc;
    if (left.token_type == ID) {
        if (!location_map.count(left.lexeme)) {
            std::cerr << "Error (parse_condition): Variable '" << left.lexeme << "' used in condition not declared at line " << left.line_no << "\n";
            exit(1);
        }
        op1_loc = location_map[left.lexeme];
    } else if (left.token_type == NUM) {
        mem[next_available] = std::stoi(left.lexeme); // Store constant
        op1_loc = next_available++;
    } else {
        // Removed token_to_string
        std::cerr << "Syntax Error (parse_condition): Expected ID or NUM for first operand, found token with lexeme '" << left.lexeme << "' at line " << left.line_no << "\n";
        exit(1);
    }

    // --- Parse Relational Operator (relop) ---
    Token rel = lexer.GetToken(); // Expect GREATER, LESS, or NOTEQUAL
    ConditionalOperatorType relop_type; // To store the parsed operator type

    if (rel.token_type == GREATER) {
        relop_type = CONDITION_GREATER; // Direct mapping
    } else if (rel.token_type == LESS) {
        relop_type = CONDITION_LESS; // Direct mapping
    } else if (rel.token_type == NOTEQUAL) {
        relop_type = CONDITION_NOTEQUAL; // Direct mapping
    } else {
        // Removed token_to_string
        std::cerr << "Syntax Error (parse_condition): Expected relational operator (>, <, !=), found token with lexeme '" << rel.lexeme << "' at line " << rel.line_no << "\n";
        exit(1);
    }

    // --- Parse Second Operand (primary) ---
    Token right = lexer.GetToken(); // Expect ID or NUM
    int op2_loc;
    if (right.token_type == ID) {
         if (!location_map.count(right.lexeme)) {
            std::cerr << "Error (parse_condition): Variable '" << right.lexeme << "' used in condition not declared at line " << right.line_no << "\n";
            exit(1);
         }
        op2_loc = location_map[right.lexeme];
    } else if (right.token_type == NUM) {
        mem[next_available] = std::stoi(right.lexeme); // Store constant
        op2_loc = next_available++;
    } else {
        // Removed token_to_string
        std::cerr << "Syntax Error (parse_condition): Expected ID or NUM for second operand, found token with lexeme '" << right.lexeme << "' at line " << right.line_no << "\n";
        exit(1);
    }

    // --- Fill the CJMP instruction node fields ---
    inst->cjmp_inst.op1_loc = op1_loc;
    inst->cjmp_inst.op2_loc = op2_loc;
    // Assign the directly mapped operator type
    inst->cjmp_inst.condition_op = relop_type;
    // inst->cjmp_inst.target is set by the calling function (parse_if_stmt, parse_while_stmt, etc.)
}

