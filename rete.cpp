#include<string>
#include<iostream>
#include<vector>
#include<list>
#include<assert.h>
using namespace std;

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
// α memory (AM): current set of working memory that pass *all* tests of a condition
//   eg. AM for [C1: (<x> ^on <y>)] contains (w1, w2, w4, w8), since they have (_ ^on _).
//       AM for [C2: (<y> ^left-of <z>)] contains (w5, w7)


// β memory: join nodes, beta memories.
// join nodes perform consistency checks _between_ conditions.
// β memory stores partial instantiation of production: combinations of WMEs which
//   match some but not all conditions of a production.

// α network also performs _intra-condition_ consistency tests: eg: (<x> ^on <x>)
// Tests can be any bolean operation.


// current working memory: relation / table(?)
// production: query
// constant test: SELECT
// If a production has c1, c2, ... cn,
//    the beta nodes perform _intermediate_ JOINS c1 x c2 .. x ck for k < n 

// the SELECT and JOINs are updated whenever the working memory (table)
// is updated.
// working memory -> α network -> β network.
// activation of node from a node in the β network is called LEFT ACTIVATION
// activation of node from a node in the α network is called RIGHT ACTIVATION

// So a β join node can have 2 types of activations:
//    - right activation | WME is added to the α memory that feeds the join node
//    - left activation  | a token is added into β memory by a parent.

// Why is Rete fast?
// 1. state-saving: after each change to WM, α and β memory states are saved
// 2. sharing of nodes: sharing can occur in the α network if multiple productions
//    have the same condition. Sharing can occur in β network if two productions
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

enum WMEFieldType { Ident = 0, Attr = 1, Val = 2, None = (-1)};

// pg 21
struct WME {
    string fields[3];
    string get_field(WMEFieldType ty) const { assert(ty != None); return fields[ty]; }
};

// pg 22
struct ReteNode {
    list<ReteNode*> children;
    ReteNode *parent;
    ReteNode() { parent = nullptr; }
    // activation from α node
    virtual void right_activation(WME w) = 0;
    virtual void left_activation(Token *t) = 0;
    virtual void left_activation(Token *t, WME w) = 0;
};

// pg 21
struct AlphaMemory {
    list<WME> items;
    vector<ReteNode *> successors;
};


// pg 14
struct ConstTestNode {
    WMEFieldType field_to_test;
    string field_must_equal;
    AlphaMemory *output_memory;
    vector<ConstTestNode *> children;
};

// pg 22
struct Token {
    Token *parent;
    WME wme;

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
        Token *new_token = new Token;
        new_token->parent = t; new_token->wme = w;
        items.push_front(new_token);
        for (ReteNode *child : children) { child->left_activation(t); }
    }
};

// pg 24
struct TestAtJoinNode {
    WMEFieldType field_of_arg1, field_of_arg2;
    int condition_number_of_arg2;
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

    void left_activation(Token *t) override {
        for(WME w : amem->items) {
            if (!this->perform_join_tests(t, w)) continue;
            for(ReteNode *child: children) child->left_activation(t, w);
        }
    }

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


struct Rete {
    ConstTestNode *alpha_top;
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
void addWME(WME w, Rete r) {
    const_test_node_activation(r.alpha_top, w);
}

int main() {
    return 0;
}
