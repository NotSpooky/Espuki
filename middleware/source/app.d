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

enum identifier = 65; // Found manually, for token.type from libdparse.

// Used to avoid exceptions as after the bootstrapping exceptions probably
// won't be used either.
enum ErrT {noError, outOfBounds, doesntExist};
struct Err {
  int type = ErrT.noError;
}

struct Parameter {
  string name;
  string dependencyValue;
}

// Represents a node of the graph in the UI.
struct CodeBox {
  @disable this();
  this (string code, uint dependencyCount = 0) {
    this.code = code;
    this.id = genVar();
    dependenciesIds = repeat("", dependencyCount).array;
  }
  void addAsArgument (CodeBox * dependant, uint inputSlotOfDependant, ref Err err) {
    dependants ~= dependant; // Note: Can create duplicates.
    auto dependantDeps = dependant.dependenciesIds;
    if (inputSlotOfDependant >= dependantDeps.length) {
      debug writeln (`Dependant has `, dependantDeps.length, `input slots,`
        ~ ` but tried to assign slot `, inputSlotOfDependant);
      err.type = ErrT.outOfBounds;
      return;
    }
    dependantDeps [inputSlotOfDependant] = this.id;
  }
  string code;
  string id;
  // Determined in dependency resolution;
  private CodeBox * [] dependants;
  private string [] dependenciesIds;
}

private void topologicalSortAux (F)(const CodeBox * box, ref bool [CodeBox*] alreadyVisited, ref bool [CodeBox*] toVisit, F output) {
  alreadyVisited[box] = true;
  toVisit.remove(box);
  foreach (dependant; box.dependants) {
    if (dependant !in alreadyVisited) {
      topologicalSortAux (dependant, alreadyVisited, toVisit, output);
    }
  }
  output (box);
}

void topologicalSort (F)(bool [CodeBox *] graphMembers, F output) {
  bool [CodeBox*] alreadyVisited;
  while (!graphMembers.empty) {
    auto box = graphMembers.byKey().front; // const.
    topologicalSortAux (box, alreadyVisited, graphMembers, output);
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
auto asChain (string nodeCode) {
  import std.string : strip;
  return nodeCode.splitter('\n').retro.map!(a => a.strip);
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

auto processOperation (F)(const CodeBox * node, ref Scope scope_, F output, ref Err err) {
  auto nodeDeps = node.dependenciesIds;
  // TODO: Add in(node)
  string[string] scopeMappingsToAdd;
  foreach(index, internalVar; nodeDeps) {
    if (!internalVar) {
      err.type = ErrT.doesntExist;
      return;
    }
    scopeMappingsToAdd["_" ~ index.to!string] = internalVar;
  }
  scope_.addLevel (scopeMappingsToAdd);
  scope (exit) scope_.scopeMap = scope_.scopeMap[0..$-1];
  auto firstLine = nodeDeps.length ? nodeDeps[0] : "";
  auto toProcess = node.code.asChain.array ~ firstLine;
  if (!node.dependants.empty) {
    output(`auto ` ~ node.id ~ ` = `);
  }
  processOperationAux (node, toProcess, scope_, output, err);
}

struct Scope {
  string [string][] scopeMap; // First dimension is the stack depth.
  void addLevel (string [string] mappings) {
    scopeMap ~= mappings;
  }
  auto opIndex (string userIdentifier, ref Err err) {
    foreach_reverse (mappings; scopeMap) {
      auto p = userIdentifier in mappings;
      if (p) return *p;
    }
    err.type = ErrT.doesntExist;
    return "";
  }
}

auto mapToScope (string str, ref Scope scope_) {
  Err err;
  auto internalName = scope_[str, err];
  if (err.type == ErrT.doesntExist) {
    return str;
  } else {
    return internalName;
  }
}

/**
 * Transforms reverseOps (a range of operations from the outer to inner 
 * functions) into output code.
 * Params:
 *   output is a function to process generated code chunks.
 * Returns:
 *   whether this produced a result that has output.
 */
private auto processOperationAux (R, F)(const CodeBox * node, ref R reverseOps, ref Scope scope_, F output, ref Err err) {
  if (reverseOps.empty) return false; // No lines

  auto currentOp = reverseOps.front;
  reverseOps.popFront();

  // Empty line, continue on next one.
  if (currentOp.empty) return false;

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
  // Argument that comes from the first input of the code box.
  // There might not be one.
  string tacitArgument; 
  // Skip until there's output.
  while ((!reverseOps.empty) && !processOperationAux(node, reverseOps, scope_, ((string a) => tacitArgument ~= a), err)) {}
  // Last operation doesn't need parens.
  bool addParens = tacitArgument || !tokenizedOp.empty;
  if (addParens) output ("(");
  // Has already dropped front, add the rest as arguments.
  if (tacitArgument) {
    output(tacitArgument);
    if (!tokenizedOp.empty) output (", ");
  }
  // Output rest of parameters as is.
  output (
      tokenizedOp
      .map!(a => a.text.to!string.mapToScope(scope_))
      .joiner(", ")
      .to!string
  );
  if (addParens) output(")");
  return true;
}

void main () {

  auto nodes = [CodeBox ("5\nmul 2"), CodeBox("10\ndivBy 2"), CodeBox("plus _1\ntostring\nwriteln 4", 2)];
  Err err;
  nodes[0].addAsArgument (&nodes[2], 0, err);
  nodes[1].addAsArgument (&nodes[2], 1, err);
  if (err.type != ErrT.noError) {
    writeln("Error adding node as argument :(");
  }
  // TODO: Check that all arguments are satisfied/have default values.
  bool [CodeBox*] graph;
  foreach(ref node; nodes) {
    graph [&node] = true;
  }
  const (CodeBox) * [] toProcess;
  void onTopoFind (const CodeBox* toAdd) {
    toProcess ~= toAdd;
  }
  topologicalSort(graph, &onTopoFind);
  Scope scope_;

  foreach_reverse(node; toProcess) {
    node.processOperation(scope_, &outputCode!string, err);
    if (err.type != ErrT.noError) {
      writeln ("Error processing node: ", node);
    }
    outputCode(";\n");
  }
  writeln(); //flush.
}
