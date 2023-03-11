#include <cassert>
#include <cstdio>

struct ListNode {
    int val;
    ListNode *next;
    ListNode(int x) : val(x), next(nullptr) {}
    ~ListNode() {
        delete next;
    }
};

struct TreeNode {
    int val;
    TreeNode *left;
    TreeNode *right;
    TreeNode(int x) : val(x), left(nullptr), right(nullptr) {}
    ~TreeNode() {
        delete left;
        delete right;
    }
};

TreeNode* sortedListToBST(ListNode* head) {
    if (head == nullptr) {
        return nullptr;
    }

    ListNode *p = head;
    ListNode *q = head;
    ListNode *prev = nullptr;
    while (q != nullptr && q->next != nullptr) {
        prev = p;
        p = p->next;
        q = q->next->next;
    }

    if (prev != nullptr) {
        prev->next = nullptr;
    }

    TreeNode *node = new TreeNode(p->val);
    if (head == p) {
        return node;
    }

    node->left = sortedListToBST(head);
    node->right = sortedListToBST(p->next);

    return node;
}

int main() {
    {
        ListNode *r = new ListNode(-10);
        r->next = new ListNode(-3);
        r->next->next = new ListNode(0);
        r->next->next->next = new ListNode(5);
        r->next->next->next->next = new ListNode(9);

        auto ret = sortedListToBST(r);
        assert(ret->val == 0);

        assert(ret->left->val == -3);
        assert(ret->left->right == nullptr);
        assert(ret->left->left->val == -10);
        assert(ret->right->val == 9);
        assert(ret->right->right == nullptr);
        assert(ret->right->left->val == 5);

        delete r;
        delete ret;
    }
    {
        auto ret = sortedListToBST(nullptr);
        assert(ret == nullptr);
    }
    return 0;
}
