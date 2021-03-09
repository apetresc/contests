#
# @lc app=leetcode id=3 lang=python3
#
# [3] Longest Substring Without Repeating Characters
#

# @lc code=start
class Solution:
    def lengthOfLongestSubstring(self, s: str) -> int:
        for l in range(min(len(s), len(set(s))), 0, -1):
            for k in range(0, len(s) - l + 1):
                ss = s[k:l+k]
                if len(set(ss)) == len(ss):
                    return l
        return 0
# @lc code=end

