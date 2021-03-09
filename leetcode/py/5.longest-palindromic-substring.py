#
# @lc app=leetcode id=5 lang=python3
#
# [5] Longest Palindromic Substring
#
# https://leetcode.com/problems/longest-palindromic-substring/description/
#
# algorithms
# Medium (29.87%)
# Likes:    8606
# Dislikes: 601
# Total Accepted:    1.1M
# Total Submissions: 3.7M
# Testcase Example:  '"babad"'
#
# Given a string s, return the longest palindromic substring in s.
# 
# 
# Example 1:
# 
# 
# Input: s = "babad"
# Output: "bab"
# Note: "aba" is also a valid answer.
# 
# 
# Example 2:
# 
# 
# Input: s = "cbbd"
# Output: "bb"
# 
# 
# Example 3:
# 
# 
# Input: s = "a"
# Output: "a"
# 
# 
# Example 4:
# 
# 
# Input: s = "ac"
# Output: "a"
# 
# 
# 
# Constraints:
# 
# 
# 1 <= s.length <= 1000
# s consist of only digits and English letters (lower-case and/or upper-case),
# 
# 
#

# @lc code=start
class Solution:
    def checkOutwards(self, s: str, i: int, l: int) -> str:
        m = i % 2
        i //= 2
        l //= 2
        if s[i - (l - 1):i + m + (l + 1)] != s[i - (l - 1):i + m + (l + 1)][::-1]:
            return ''
        while i - l >= 0 and i + m + l < len(s) and s[i + m - l] == s[i + m + l]:
            l += 1
        return s[i + m - l:i + m + l]

    def longestPalindrome(self, s: str) -> str:
        r = s[0]
        for i in range(1, (len(s) * 2) - 2):
            p = self.checkOutwards(s, i, len(r) + 1)
            if len(p) > len(r):
                r = p
        return r

print(Solution().longestPalindrome("babad"))
print(Solution().longestPalindrome("cbbd"))
print(Solution().longestPalindrome("a"))
print(Solution().longestPalindrome("ac"))
print(Solution().longestPalindrome("bb"))
print(Solution().longestPalindrome("fdjslghabadabah"))
# @lc code=end

