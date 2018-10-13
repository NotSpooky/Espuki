// TODO: Repeat macro. Eg:
/*
ovejaNodes[3] = kumiko;
sleepyNodes[3] = kumiko;
gets translated to
(ovejaNodes, sleepyNodes) =} _[3] = kumiko;
*/

import std.variant;
import std.range;
import std.algorithm;
import std.conv : to;
debug import std.stdio;

enum identifier = 65; // Found manually, for token.type.

struct CodeBox {
  string code;
  CodeBox * [] dependants;
}

void topologicalSort (F)(const CodeBox * box, ref bool [CodeBox*] alreadyVisited, ref bool [CodeBox*] toVisit, F output) {
  alreadyVisited[box] = true;
  toVisit.remove(box);
  foreach (dependant; box.dependants) {
    if (dependant !in alreadyVisited) {
      topologicalSort (dependant, alreadyVisited, toVisit, output);
    }
  }
  output (box.code);
}

void process (F)(bool [CodeBox *] graphMembers, F output) {
  bool [CodeBox*] alreadyVisited;
  while (!graphMembers.empty) {
    auto box = graphMembers.byKey().front;
    topologicalSort (box, alreadyVisited, graphMembers, output);
  }
}

void outputCode (R)(R input) {
  import std.stdio;
  write (input);
}

/**
 * Transforms the text corresponding to a node into a range of
 * those lines in reverse.
 */
auto asChain (string nodeAsText) {
  import std.string : strip;
  return nodeAsText.splitter('\n').retro.map!(a => a.strip);
}

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
  if (reverseOps.empty) return; // No lines

  auto currentOp = reverseOps.front;
  reverseOps.popFront();

  // Empty line, continue on next one.
  if (currentOp.empty) return;


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
}

void main () {

  auto nodes = [CodeBox ("5\nmul 2"), CodeBox("10\ndivBy 2"), CodeBox("plus 4\ntostring\nwriteln 4")];
  nodes[0].dependants = [&nodes[2]];
  nodes[1].dependants = [&nodes[2]];
  bool [CodeBox*] graph;
  foreach(ref node; nodes) {
    graph [&node] = true;
  }
  string[] toProcess;
  void onTopoFind (string toAdd) {
    toProcess ~= toAdd;
  }
  process(graph, &onTopoFind);

  foreach_reverse(node; toProcess) {
    node.asChain.processOperation(&outputCode!string);
    outputCode(";\n");
  }
  writeln(); //flush.
}
