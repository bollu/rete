#include<string>
#include<iostream>
#include<vector>
#include<list>
#include<assert.h>
using namespace std;

// Production Matching for Large Learning Systems
// http://reports-archive.adm.cs.cmu.edu/anon/1995/CMU-CS-95-113.pdf
// WM: working memory - current situation of the system
//  - WME: working memory element
// eg. block world:
// w1: (B1 ^on B2)
// w2: (B1 ^on B3)
// w3: (B1 ^color red)
// w4: (B2 ^on table)
// w5: (B2 ^left-of B3)
// w6: (B2 ^color blue)
// w7: (B3 ^left-of B4)
// w8: (B3 ^on table)
// w9: (B3 ^color red)
// wn: (id ^attr val)


// production can contain varibles, in angle brackets: <x>
// (find-stack-of-two-blocks-to-the-left-of-a-red-block
// (<x> ^on <y>) (<y> ^left-of <z>) (<z> ^color red) --> ... RHS ... )
// eg. C1: (<x> ^on <y>)
//     C2: (<y> ^left-of <z>)
//     C3: (<z> %color red)


// A production can match with the current WM if all the conditions match
// with intems in WM, with variables bound consistently.
// Create dataflow network for conditions.
// alpha memory (AM): current set of working memory that pass *all* tests of a condition
//   eg. AM for [C1: (<x> ^on <y>)] contains (w1, w2, w4, w8), since they have (_ ^on _).
//       AM for [C2: (<y> ^left-of <z>)] contains (w5, w7)


// beta memory: join nodes, beta memories.
// join nodes perform consistency checks _between_ conditions.
// beta memory stores partial instantiation of production: combinations of WMEs which
//   match some but not all conditions of a production.

// alpha network also performs _intra-condition_ consistency tests: eg: (<x> ^on <x>)
// Tests can be any bolean operation.


// current working memory: relation / table(?)
// production: query
// constant test: SELECT
// If a production has c1, c2, ... cn,
//    the beta nodes perform _intermediate_ JOINS c1 x c2 .. x ck for k < n 

// the SELECT and JOINs are updated whenever the working memory (table)
// is updated.
// working memory -> alpha network -> beta network.
// activation of node from a node in the beta network is called LEFT ACTIVATION
// activation of node from a node in the alpha network is called RIGHT ACTIVATION

// So a beta join node can have 2 types of activations:
//    - right activation | WME is added to the alpha memory that feeds the join node
//    - left activation  | a token is added into beta memory by a parent.

// Why is Rete fast?
// 1. state-saving: after each change to WM, alpha and beta memory states are saved
// 2. sharing of nodes: sharing can occur in the alpha network if multiple productions
//    have the same condition. Sharing can occur in beta network if two productions
//    have similar first few conditions.

// data WME id attr val = WME id attr val deriving(Eq, Ord)
// brackets :: [String] -> String; brackets s = "("<>List.intercalate " " s<>")"
// instance (Show id, Show attr, Show val) => Show (WME id attr val) where
//   show (WME a b c) = brackets [show a, "^" <> show b, show c]
// 
// -- PM: production memory
// data Production cond act = Production cond act deriving (Eq, Ord)
// instance (Show cond, Show act) => Show (Production cond act) where
//   show (Production cond act) = brackets [show cond, "-->", show act]
struct Token;

enum WMEFieldType { None = (-1), Ident = 0, Attr = 1, Val = 2, NumFields=3 };

// pg 21
struct WME {
    string fields[3];
    string get_field(WMEFieldType ty) const { assert(ty != None); return fields[ty]; }

    WME(string id, string attr, string val) {
        fields[WMEFieldType::Ident] = id;
        fields[WMEFieldType::Attr] = attr;
        fields[WMEFieldType::Val] = val;
    }
};

// pg 22
struct ReteNode {
    list<ReteNode*> children;
    ReteNode *parent;
    ReteNode() { parent = nullptr; }
    // activation from alpha node
    virtual void right_activation(WME w) = 0;
    // activation from beat node
    virtual void left_activation(Token *t) = 0;
    virtual void left_activation(Token *t, WME w) = 0;
};

// pg 21
struct AlphaMemory {
    list<WME> items;
    list<ReteNode *> successors;
};


// pg 14
struct ConstTestNode {
    WMEFieldType field_to_test;
    string field_must_equal;
    AlphaMemory *output_memory;
    vector<ConstTestNode *> children;

    static ConstTestNode *dummy_top() {
        ConstTestNode *node = new ConstTestNode;
        node->field_to_test = WMEFieldType::None;
        node->field_must_equal = "-42";
        node->output_memory = nullptr;
        return node;
    }
};

// pg 22
struct Token {
    Token *parent; // items [0..i-1]
    WME wme; // item i

    Token(WME wme, Token *parent) : wme(wme), parent(parent) {}

    // implicitly stated on pages:
    // - pg 20
    // - pg 25 {With list-form tokens,the following statement is really a loop}
    WME index(int i) {
        assert(i >= 0);
        if (i == 0) return wme;
        assert(parent != nullptr);
        return parent->index(i - 1);
    }
};


// pg 22
struct BetaMemory : public ReteNode {
    list<Token*> items;

    void right_activation(WME w) override { assert(false && "unimplemented"); }
    void left_activation(Token *t) override { assert(false && "unimplemented"); }
    // pg 23: dodgy! the types are different from BetaMemory and their children
    // updates
    void left_activation(Token *t, WME w) override {
        Token *new_token = new Token(w, t);
        // new_token->parent = t; new_token->wme = w;
        items.push_front(new_token);
        for (ReteNode *child : children) { child->left_activation(t); }
    }

    void insert_token_at_head(Token *t) {
    }
};

// pg 24
struct TestAtJoinNode {
    WMEFieldType field_of_arg1, field_of_arg2;
    int condition_number_of_arg2;

    bool operator == (const TestAtJoinNode &other) const {
        return field_of_arg1 == other.field_of_arg1 &&
            field_of_arg2 == other.field_of_arg2 &&
            condition_number_of_arg2 == other.condition_number_of_arg2;
    }
};

/// pg 24
struct JoinNode : public ReteNode {
    AlphaMemory *amem;
    list<TestAtJoinNode> tests;

    JoinNode() : ReteNode(), amem(nullptr) {};

    // pg 24
    void right_activation(WME w) override {
        BetaMemory *beta_parent = dynamic_cast<BetaMemory *>(parent);
        assert(beta_parent != nullptr);
        for (Token *t : beta_parent->items) {
            if (!this->perform_join_tests(t, w)) continue;
            for(ReteNode *child: children) child->left_activation(t, w);
        }
    }

    // pg 25
    void left_activation(Token *t) override {
        for(WME w : amem->items) {
            if (!this->perform_join_tests(t, w)) continue;
            for(ReteNode *child: children) child->left_activation(t, w);
        }
    }

    void left_activation(Token *t, WME w) override { 
        assert(false && "unimplemented");
    }

    // pg 25
    bool perform_join_tests(Token *t, WME w) const {
        for (TestAtJoinNode test : tests) {
            string arg1 = w.get_field(test.field_of_arg1);
            WME wme2 = t->index(test.condition_number_of_arg2);
            string arg2 = wme2.get_field(test.field_of_arg2);
            if (arg1 != arg2) return false;
        }
        return true;
    }
};


// no page; hold all global state
struct Rete {
    ConstTestNode *alpha_top;
    ReteNode *dummy_top_node;

    // inferred from page 35: build_or_share_alpha memory:
    // { initialize am with any current WMEs }
    // presupposes knowledge of a collection of WMEs
    vector<WME> working_memory;
};



// pg 21
void alpha_memory_activation(AlphaMemory *node, WME w) {
    node->items.push_front(w);
    for (ReteNode *child : node->successors) child->right_activation(w);

}

// pg 15
// return whether test succeeded or not.
bool const_test_node_activation(ConstTestNode *node, WME w) {
    if (node->field_to_test != WMEFieldType::None) {
        if (w.get_field(node->field_to_test) != node->field_must_equal) {
            return false;
        }
    }

    if (node->output_memory) {
        alpha_memory_activation(node->output_memory, w);
    }
    for (ConstTestNode *c : node->children) {
        const_test_node_activation(c, w);
    }
    return true;
}

// pg 14
void addWME(WME w, Rete &r) {
    const_test_node_activation(r.alpha_top, w);
}

// pg 38
void update_new_node_with_matches_from_above(ReteNode *newNode) {
    ReteNode *parent = newNode->parent;
    assert(parent != nullptr);

    if (BetaMemory *bm = dynamic_cast<BetaMemory *>(parent)) {
        for (Token *t: bm->items) {
            newNode->left_activation(t);
        }
    } else if (JoinNode *join = dynamic_cast<JoinNode *>(parent)) {
        list<ReteNode *> savedListOfChildren = parent->children;

        // WTF?
        join->children = { newNode };
        for(WME item : join->amem->items) { join->right_activation(item); }
        join->children = savedListOfChildren;
    } else { assert(false && "unknown parent type"); }

}

// pg 34
BetaMemory *build_or_share_beta_memory_node(ReteNode *parent) {
    for (ReteNode *child : parent->children) {
        BetaMemory *beta = dynamic_cast<BetaMemory*>(child);
        if (beta) return beta;
    }
    BetaMemory *newbeta = new BetaMemory;
    newbeta->parent = parent;
    //newbeta->children = nullptr;
    //newbeta->items = nullptr;
    parent->children.push_front(newbeta);
    update_new_node_with_matches_from_above(newbeta);
    return newbeta;
}

// pg 34
JoinNode *build_or_share_join_node(ReteNode *parent, AlphaMemory *am,
        list<TestAtJoinNode> tests) {
    assert(parent != nullptr);
    for (ReteNode *child : parent->children) {
        JoinNode *join = dynamic_cast<JoinNode*>(child);
        if (join && join->amem == am && join->tests == tests) return join;
    }

    JoinNode *newjoin = new JoinNode;
    newjoin->parent = parent; parent->children.push_front(newjoin);
    //newjoin->children = nullptr
    newjoin->tests = tests; newjoin->amem = am;
    am->successors.push_front(newjoin);
    return newjoin;
}

// inferred from discussion
enum FieldType {
    Const = 0,
    Var = 1
};

// inferred from discussion
struct Field {
    FieldType type;
    string v;

    static Field var(string name) {
        Field f; f.type = FieldType::Var; f.v = name; return f;
    }

    static Field constant(string name) {
        Field f; f.type = FieldType::Const; f.v = name; return f;
    }
};

// inferred from discussion
struct Condition {
    Field attrs[NumFields];

    Condition(Field ident, Field attr, Field val ) {
        attrs[0] = ident; attrs[1] = attr; attrs[2] = val;
    }
};

// implicitly defined on pg 35
void lookup_earlier_cond_with_field(const vector<Condition> &earlierConds, 
        string v, int *i, int *f2) {
    // i: largest i such that ith condition
    // in earlier-condition contains
    // a field f2 in which v occurs
    *i = earlierConds.size() - 1;
    *f2 = -1;

    auto it = earlierConds.rend();
    bool found = false;
    while(it != earlierConds.rbegin()) {
        for (int j = 0; j < NumFields; ++j) {
            if (it->attrs[j].type != FieldType::Var) continue;
            if (it->attrs[j].v == v) { 
                *f2 = j;
                return;
            }
        }
        i--;
    }
    *i = *f2 = -1;
}

// pg 35
// pg 35: supposedly, nearness is not a _hard_ requiement.
list<TestAtJoinNode> get_join_tests_from_condition(Condition c, 
        vector<Condition> earlierConds) {
    list<TestAtJoinNode> result;

    for(int f = 0; f < NumFields; ++f) {
        if (c.attrs[f].type != FieldType::Var) continue;
        // each occurence of variable v
        const string v = c.attrs[f].v;
        int i, f2;
        lookup_earlier_cond_with_field(earlierConds, v, &i, &f2);
        // nothing found
        if (i == -1)  { assert(f2 == -1); continue; }
        assert(i != -1); assert(f2 != -1);
        TestAtJoinNode test;
        test.field_of_arg1 = (WMEFieldType) f;
        test.condition_number_of_arg2 = i;
        test.field_of_arg2 = (WMEFieldType) f2;
        result.push_back(test);
    }
    return result;
};

// page 36
ConstTestNode *build_or_share_constant_test_node(ConstTestNode *parent, 
        WMEFieldType f, string sym) {
    assert(parent != nullptr);
    // look for pre-existing node
    for (ConstTestNode *child: parent->children) {
        if (child->field_to_test == f && child->field_must_equal == sym) {
            return child;
        }
    }
    // build a new node
    ConstTestNode *newnode = new ConstTestNode;
    parent->children.push_back(newnode);
    newnode->field_to_test = f; newnode->field_must_equal = sym;
    newnode->output_memory = nullptr; 
    // newnode->children = nullptr;
    return newnode;
}

// implied in page 35: build_or_share_alpha_memory.
bool wme_passes_constant_tests(WME w, Condition c) {
    for(int f = 0; f < NumFields; ++f) {
        if (c.attrs[f].type != FieldType::Const) continue;
        if (c.attrs[f].v != w.fields[f]) return false;
    }
    return true;
}

// pg 35: dataflow version
AlphaMemory *build_or_share_alpha_memory_dataflow(Condition c, Rete &r) {
    ConstTestNode *currentNode = r.alpha_top;
    for (int f = 0; f < NumFields; ++f) {
        if (c.attrs[f].type != FieldType::Const) continue;
        const string sym = c.attrs[f].v;
        currentNode = build_or_share_constant_test_node(currentNode, 
                (WMEFieldType)f, sym);
    }

    if (currentNode->output_memory != nullptr) {
        return currentNode->output_memory;
    }
    assert(currentNode->output_memory == nullptr);
    currentNode->output_memory = new AlphaMemory;
    // initialize AM with any current WMEs
    for (WME w: r.working_memory) {
        // check if wme passes all constant tests
        if (wme_passes_constant_tests(w, c)) {
            alpha_memory_activation(currentNode->output_memory, w);
        }
    }
    return currentNode->output_memory;
};

// page 36: hash version
AlphaMemory *build_or_share_alpha_memory_hashed(Condition c, Rete &r) {
    assert(false && "unimplemented");
};


struct ProductionNode : public ReteNode {
    string rhs;

    // activation from alpha node
    void right_activation(WME w)
    { assert(false && "have not thought about it"); }
    // activation from beat node
    void left_activation(Token *t)
    { assert(false && "have not thought about it"); }
    void left_activation(Token *t, WME w)
    { assert(false && "have not thought about it"); }
};

// pg 37
// - inferred type of production node: 
ProductionNode *add_production(vector<Condition> lhs, string rhs, Rete &r) {
    // pseudocode: pg 33
    // M[1] <- dummy-top-node
    // build/share J[1] (a child of M[1]), the join node for c[1]
    // for i = 2 to k do
    //     build/share M[i] (a child of J[i-1]), a beta memory node
    //     build/share J[i] (a child of M[i]), the join node for ci
    // make P (a child of J[k]), the production node

    // PITFALL: this says "dummy top node", not srure if this is the
    // same as alpha_top.
    ReteNode *currentNode = r.dummy_top_node;
    vector<Condition> earlierConds;

    list<TestAtJoinNode> tests = get_join_tests_from_condition(lhs[0], earlierConds);
    AlphaMemory *am = build_or_share_alpha_memory_dataflow(lhs[0], r);
    currentNode = build_or_share_join_node(currentNode, am, tests);

    for(int i = 1; i < lhs.size(); ++i) {
        // get the current beat memory node M[i]
        currentNode = build_or_share_beta_memory_node(currentNode);
        // get the join node J[i] for condition c[u[
        earlierConds.push_back(lhs[i-1]);
        tests = get_join_tests_from_condition(lhs[i], earlierConds);
        am = build_or_share_alpha_memory_dataflow(lhs[i], r);
        currentNode = build_or_share_join_node(currentNode, am, tests);
    }

    // build a new production node, make it a child of current node
    ProductionNode *prod = new ProductionNode; 
    prod->rhs = rhs;
    currentNode->children.push_front(prod);
    // update new-node-with-matches-from-above (the new production node)
    update_new_node_with_matches_from_above(prod);
    return prod;
}

struct ReteDummyTopNode : public ReteNode {
    void right_activation(WME w) { assert(false && "unimplement ReteDummyTopNode"); }
    // activation from beat node
    void left_activation(Token *t) { assert(false && "unimplement ReteDummyTopNode"); }
    void left_activation(Token *t, WME w) { assert(false && "unimplement ReteDummyTopNode"); }
};

int main() {
    // w1: (B1 ^on B2)
    // w2: (B1 ^on B3)
    // w3: (B1 ^color red)
    // w4: (B2 ^on table)
    WME w1("B1", "on", "B2");
    WME w2("B1", "on", "B3");
    WME w3("B1", "color", "red");
    WME w4("B2", "on", "table");

    Rete rete;
    rete.alpha_top = ConstTestNode::dummy_top();
    rete.dummy_top_node = new ReteDummyTopNode();
    rete.working_memory.push_back(WME("B1", "on", "B2"));
    rete.working_memory.push_back(WME("B1", "on", "B3"));
    rete.working_memory.push_back(WME("B1", "color", "red"));
    rete.working_memory.push_back(WME("B1", "on", "table"));
    rete.working_memory.push_back(WME("B2", "left-of", "B3"));
    rete.working_memory.push_back(WME("B2", "color", "blue"));
    rete.working_memory.push_back(WME("B3", "left-of", "B4"));
    rete.working_memory.push_back(WME("B3", "on", "table"));
    rete.working_memory.push_back(WME("B3", "color", "red"));
    rete.working_memory.push_back(WME("id", "attr", "val"));

    add_production(std::vector<Condition>({Condition(Field::var("x"),
                    Field::constant("on"), Field::var("y"))}),
            "prod2", rete);

    add_production(std::vector<Condition>(
                {Condition(Field::var("x"), Field::constant("on"), Field::var("y")),
                Condition(Field::var("y"), Field::constant("left-of"), Field::var("z")),
                Condition(Field::var("z"), Field::constant("color"), Field::constant("red"))
                }), "prod2", rete);

    // ProductionNode *p = add_production()
    return 0;
}
