/*
 * @lc app=leetcode id=9 lang=cpp
 *
 * [9] Palindrome Number
 *
 * https://leetcode.com/problems/palindrome-number/description/
 *
 * algorithms
 * Easy (49.83%)
 * Likes:    3097
 * Dislikes: 1708
 * Total Accepted:    1.2M
 * Total Submissions: 2.4M
 * Testcase Example:  '121'
 *
 * Given an integer x, return true if x is palindrome integer.
 * 
 * An integer is a palindrome when it reads the same backward as forward. For
 * example, 121 is palindrome while 123 is not.
 * 
 * 
 * Example 1:
 * 
 * 
 * Input: x = 121
 * Output: true
 * 
 * 
 * Example 2:
 * 
 * 
 * Input: x = -121
 * Output: false
 * Explanation: From left to right, it reads -121. From right to left, it
 * becomes 121-. Therefore it is not a palindrome.
 * 
 * 
 * Example 3:
 * 
 * 
 * Input: x = 10
 * Output: false
 * Explanation: Reads 01 from right to left. Therefore it is not a
 * palindrome.
 * 
 * 
 * Example 4:
 * 
 * 
 * Input: x = -101
 * Output: false
 * 
 * 
 * 
 * Constraints:
 * 
 * 
 * -2^31 <= x <= 2^31 - 1
 * 
 * 
 * 
 * Follow up: Could you solve it without converting the integer to a string?
 */

// @lc code=start
#include <iostream>
#include <math.h>

class Solution {
public:
    bool isPalindrome(int x) {
        if (x < 0) {
            return x == 0;
        }
        int num_digits = static_cast<int>(log10(x)) + 1;
        while (num_digits > 1) {
            int first_digit = static_cast<int>(x / pow(10, num_digits - 1));
            int last_digit = x % 10;
            if (first_digit != last_digit) {
                return false;
            }
            x %= static_cast<int>(pow(10, num_digits - 1));
            x /= 10;
            num_digits -= 2;
        }
        return true;
    }
};
// @lc code=end

int main() {
    Solution s = Solution();
    std::cout << s.isPalindrome(0) << std::endl;
    std::cout << s.isPalindrome(121) << std::endl;
    std::cout << s.isPalindrome(10) << std::endl;
    std::cout << s.isPalindrome(-121) << std::endl;
    std::cout << s.isPalindrome(12421) << std::endl;
    std::cout << s.isPalindrome(12321) << std::endl;
    std::cout << s.isPalindrome(1000021) << std::endl;

    return 0;
}