#include <iostream>
#include <vector>

using namespace std;

class Solution {
public:
  vector<vector<int>> subsetsWithDup(vector<int> &nums) {
    vector<vector<int>> result;
    for (int i = 0; i < 1 << nums.size(); i++) {
      vector<int> r;
      int j = i;
      int k = 0;
      while (j > 0) {
        if (j % 2 == 1) {
          r.push_back(nums[k]);
        }
        k++;
        j>>=1;
      }
      sort(r.begin(), r.begin() + r.size());
      result.push_back(r);
    }
    sort(result.begin(), result.begin() + result.size());

    vector<vector<int>> result_set = {{}};
    vector<int> last = {};
    for (vector<vector<int>>::iterator it = result.begin(); it != result.end(); ++it) {
      if (last == *it) {
        continue;
      }
      last = *it;
      result_set.push_back(*it);
    }

    return result_set;
  }
};

int main() {
  Solution s;
  vector<int> input = {9, 1, 2, 8, 2, 1, 3};
  vector<vector<int>> result = s.subsetsWithDup(input);

  for (vector<vector<int>>::iterator it = result.begin(); it != result.end(); ++it) {
    for (vector<int>::iterator r = it->begin(); r != it->end(); ++r) {
      cout << *r << " ";
    }
    cout << endl;
  }

  return 0;
}
