#include <iostream>                  // for std::cout
#include <utility>                   // for std::pair
#include <algorithm>                 // for std::for_each
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/depth_first_search.hpp>

using namespace boost;

int main(int,char*[])
{
  // create a typedef for the Graph type
  typedef adjacency_list<vecS, vecS, bidirectionalS> Graph;

  const int num_vertices = 1000000;
  Graph g(num_vertices);

  clock_t begin = clock();
  for (int i = 0; i < num_vertices - 1; i++) {
     add_edge(i, i + 1, g);
  }
  std::cout << "Time to create graph "
      << (double(clock() - begin) / CLOCKS_PER_SEC) << "\n";

  begin = clock();
  default_dfs_visitor vis;
  depth_first_search (g, visitor (vis));
  std::cout << "Depth-First-Search "
      << (double(clock() - begin) / CLOCKS_PER_SEC) << "\n";

  return 0;
}
