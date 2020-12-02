#
# @lc app=leetcode id=2 lang=python3
#
# [2] Add Two Numbers
#

from itertools import zip_longest

# @lc code=start

# Definition for singly-linked list.
class ListNode:
    def __init__(self, val=-1, next=None):
        self.val = val
        self.next = next

def to_l(ln):
    while ln:
        yield ln.val
        ln = ln.next
    return

def to_ln(l):
    head = ListNode(l[0])
    tail = head
    for e in l[1:]:
        tail.next = ListNode()
        tail = tail.next
        tail.val = e
    return head

class Solution:
    def addTwoNumbers(self, l1, l2):
        carry = 0
        r = []
        for d1, d2 in zip_longest(to_l(l1), to_l(l2), fillvalue=0):
            s = carry + d1 + d2
            carry = s // 10
            r.append(s % 10)
        if carry > 0:
            r.append(carry)

        print(r)
        return to_ln(r)

# @lc code=end
