var app = angular.module('containers', []);

var MEAN_PERCENT = 0;
var MEAN_MS = 1;

app.
run(function($rootScope) {
   $rootScope.display = MEAN_PERCENT;
}).

factory('Reftime', function() {
   /**
    * A category of tests if generally for a given container and element_type.
    * They correspond to a table displayed in the output. They include their
    * own reference times and list of tests.
    * @constructor
    * @param {str} name   The name of the category.
    */
   function Category(name) {
      this.name = name;
      this.containers = [];  // List of containers in this category
      this.test_names = [];  // List of tests run for any container in this
                             // category
      this.is_ref_test = {}; // List of tests that are references

   }

   /**
    * Get the list of categories and their attributes from the data generated
    * by the tests.
    * @return {array<Category>} the list of categories.
    */
   Category.compute = function() {
      var result = [];
      var seen = {};   // {name->Category}

      angular.forEach(data.tests, function(container) {
         var cat = seen[container.category];
         if (!cat) {
            cat = seen[container.category] = new Category(container.category);
            result.push(cat);
         }

         cat.containers.push(container);

         // Compute mean execution time for each tests
         angular.forEach(container.tests, function(test) {
            var g = 0.0;
            angular.forEach(test.duration, function(d) {
               g += d;
            });
            test.mean_duration = g / test.duration.length;
            test.cumulated = g;
         });
      });

      angular.forEach(result, function(cat) {
         var seen = {};  // Set of tests known for this category
         var refs = {};  // Name -> test   the reference tests
         var ref_test = undefined;  // Current ref test (a Test object)

         angular.forEach(cat.containers, function(container) {
            angular.forEach(container.tests, function(test, name) {
               if (!seen[name]) {
                  seen[name] = {};
                  cat.test_names.push(name);
               }
               if (test.group) {
                  cat.is_ref_test[name] = true;
                  ref_test = refs[name];
                  if (!ref_test) {
                     ref_test = test;
                     refs[name] = ref_test;
                  }
               }
               test.ref = ref_test;
            });
         });
      });

      return result;
   };

   /**
    * This class is used to compute percents compared to various reference
    * times.
    */
   function Reftime() {
      this.data = {
         categories: Category.compute(),  // list of Category
         repeat_count: data.repeat_count,
         items_count: data.items_count
      };
   }

   /**
    * Return the computed percent value for a given test
    */
   Reftime.prototype.compute_percent = function(test_name, test, category) {
      var rt = test.ref;
      test.mean_percent = (
         (test.mean_duration / rt.mean_duration) * 100).toFixed(0);
      test.mean_duration_str = (test.mean_duration * 1000).toFixed(2);
   };

   return new Reftime;
}).

controller('ResultsCtrl', function($scope, Reftime) {
   $scope.data = Reftime.data;  // from global variable
}).

controller('HeaderCtrl', function(Reftime, $scope) {
   $scope.data = Reftime.data;
   $scope.percents = [
      {value: MEAN_PERCENT, text: 'As percent'},
      {value: MEAN_MS, text: 'As milliseconds'}
   ];
}).

/**
 * Display memory usage for a test
 */
directive('ctMem', function() {
   return {
      scope: {
         testname: '=ctMem',
         container: '='
      },
      controller: function($scope, Reftime, $rootScope) {
         $scope.test = $scope.container.tests[$scope.testname];
      },
   template:
      '<div class="mem" ng-if="test && $root.show_test_allocs">' +
         '<div ng-if="test.allocated">{{test.allocated | kb}}</div>' +
         '<div ng-if="test.allocs">+{{test.allocs}}</div>' +
         '<div ng-if="test.reallocs">@{{test.reallocs}}</div>' +
         '<div ng-if="test.frees">-{{test.frees}}</div>' +
      '</div>'
   };
}).

directive('ctDuration', function() {
   return {
      scope: {
         testname: '=ctDuration',
         aspercent: '=',
         container: '='
      },
      controller: function($scope, Reftime, $rootScope) {
         $scope.test = $scope.container.tests[$scope.testname];
         if ($scope.test) {
            Reftime.compute_percent(
               $scope.testname,
               $scope.test,
               $scope.container.category);
         }
      },
   template: '<span ng-if="test"' +
           ' ng-class="{worse:test.mean_percent>105, comment:test.comment}"' +
           ' title="{{test.comment}}">' +

         '<span ng-if="$root.display==0"' +
              ' ng-class="{worse:test.mean_percent>105}">' +
              '{{test.mean_percent}}%</span>' +

         '<span ng-if="$root.display==1"' +
              ' ng-class="{worse:test.mean_percent>105}">' +
              '{{test.mean_duration_str}}</span>' +
         '</span>'
   };
}).

filter('kb', function() {
   return function(value) {
      if (!value) {
         return '0';
      } else if (value > 1000) {
         return (value / 1000).toFixed(0) + 'kb';
      } else {
         return value + 'b';
      }
   };
});
