#include <iostream>
#include <set>
#include <vector>

using namespace std;

class Solution {
public:
    int arrayNesting(vector<int>& nums) {
        int m = 0;
        
        
        for (int i = 0; i < nums.size(); i++) {
            int j = i;
            int c = 0;

            do {
                int t = nums[j];
                nums[i] = -1;
                c += 1;
                j = t;
            } while (nums[j] >= 0);
            m = max(m, c);
        }

        return m;
    }
};

int main() {
  Solution s;
  vector<int> input = {0,2,1};
  int result = s.arrayNesting(input);

  cout << result << endl;
  return 0;
}
