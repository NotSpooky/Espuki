import std.variant;
import std.range;
import std.algorithm;
import std.conv : to;
debug import std.stdio;

enum identifier = 65; // Found manually, for token.type.

void outputCode (R)(R input) {
  import std.stdio;
  write (input);
}

string getNode () {
  return `"Hello world"` ~ "\nsplit" ~ "\nwriteln 5 7";
}

/**
 * Transforms the text corresponding to a node into a range of
 * those lines in reverse.
 */
auto asChain (string nodeAsText) {
  import std.string : strip;
  return nodeAsText.splitter('\n').retro.map!(a => a.strip);
}

/**
 * Transforms the text correspoding to a line into a range of input AST nodes.
 */
auto lineAsInputAST (string line) {
  import dparse.lexer;
  
}
/+
auto parseLine (string line, string lastValue = null) {
  auto tokens = byToken(line);
  if(tokens.empty) return "";
  auto operation = tokens.front;
  auto opType = operation.type; // D token type.
  // This assert might not be needed.
  assert(opType.isLiteral || opType == identifier);
  auto expression = [operation.text];
  if (lastValue) {
    expression ~= lastValue;
  }
  return processOperation (
      expression ~ tokens.dropOne.map!(a => a.text).array
      , &writeln!string
      );
}+/


// Might be better to make it shared.
private uint lastId = 0;

/**
 * Generates a unique variable name.
 * Doesn't insert anything into the AST (such as a declaration).
 * Returns: The name of an unique variable.
 */
auto genVar () {
  scope (exit) lastId++;
  return `var` ~ lastId.to!string;
}


/**
 * Transforms reverseOps (a range of operations from the outer to inner 
 * functions) into output code.
 * Params:
 *   output is a function to process generated code chunks.
 */
auto processOperation (R, F)(R reverseOps, F output) {
  if (reverseOps.empty) return ""; // No lines

  auto currentOp = reverseOps.front;
  reverseOps.popFront();

  // Empty line, continue on next one.
  if (currentOp.empty) return processOperation(reverseOps, output);


  import dparse.lexer;
  // Set up the lexer for this line.
  auto lexerConfig = LexerConfig();
  lexerConfig.stringBehavior = StringBehavior.source;         // Get strings as is.
  lexerConfig.whitespaceBehavior = WhitespaceBehavior.skip;
  lexerConfig.commentBehavior = CommentBehavior.noIntern;
  auto cache = StringCache (64);

  auto tokenizedOp = currentOp.byToken (lexerConfig, &cache); // Current line tokens.
  auto currentOpHeader = tokenizedOp.front.text;              // Current line first token.
  tokenizedOp.popFront();

  output(currentOpHeader);
  // Last operation doesn't need parens.
  if (!reverseOps.empty) output ("(");
  // Has already dropped front, add the rest as arguments.
  processOperation(reverseOps, output);
  if (!tokenizedOp.empty) output(", ");
  // Output rest of parameters as is.
  output (tokenizedOp.map!(a => a.text.to!string).joiner(", ").to!string);
  if (!reverseOps.empty) output(")");
  /+
  auto first = expression.front;
  auto tail  = expression.dropOne; 
  auto outputName = genVar();
  auto outputDecl = `auto ` ~ outputName ~ ` = `;
  if (tail.empty) {
    output(outputDecl ~ first ~ ';');
  } else {
    output(outputDecl ~ first ~ '(' ~ `);`);
  }
  return outputName;
  +/
  return "";
}



/+
auto functionChain (R)(R input) {
  import std.string;
  auto steps = input.map!(a => a.strip).retro;
  return steps;
}

// TODO: Optimize concatenation.
auto functionChainRec (R)(R steps) {
  if (steps.empty) {
    return "";
  } else {
    return steps.front.to!string ~ '(' ~ functionChainRec (steps.dropOne) ~ ')'; 
  }
}+/

void main () {
  getNode.asChain.processOperation(&outputCode!string);
  writeln(); //flush.
}
