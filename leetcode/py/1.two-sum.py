#
# @lc app=leetcode id=1 lang=python3
#
# [1] Two Sum
#
from typing import List, Dict

# @lc code=start
class Solution:
    def twoSum(self, nums: List[int], target: int) -> List[int]:
        targets: Dict[int, int] = {}
        for i, n in enumerate(nums):
            if n in targets:
                return [i, targets[n]]
            else:
                targets[target - n] = i

        return []
# @lc code=end

