var app = angular.module('containers', []);

var test_name_to_reftime = {
   'fill': 'fill',
   'copy': 'fill',
   'cursor loop': 'cursor loop',
   'for-of loop': 'cursor loop',
   'count_if': 'cursor loop'
};

// Tests before which we display a bold border
var ref_test_names = {
   'fill': true,
   'cursor loop': true
};

app.
run(function($rootScope) {
   $rootScope.as_percent = true;
}).

factory('Reftime', function() {
   /**
    * This class is used to compute percents compared to various reference
    * times.
    */
   function Reftime() {
      var ref = this;

      ref.data = {
         // For each category, include the following data:
         //  reftimes: {testname -> duration}   reference time for each test
         //  containers: [list of containers]  the lists of tests that were run
         categories: {},
         test_names: [
            'fill',
            'copy',
            'cursor loop',
            'for-of loop',
            'count_if'
         ],
         repeat_count: data.repeat_count,
         items_count: data.items_count
      };

      angular.forEach(data.tests, function(container) {
         // Compute mean execution time for each tests
         angular.forEach(container.tests, function(test) {
            var g = 0.0;
            angular.forEach(test.duration, function(d) {
               g += d;
            });
            test.mean_duration = g / test.duration.length;
         });

         // Group data by category (these correspond to the various tables
         // displayed in the output).
         var e = container.category;
         var cat = ref.data.categories[e];
         if (!cat) {
            cat = ref.data.categories[e] = {
               reftimes: {},
               containers: []};
         }
         cat.containers.push(container);
      });

      this.set_reftimes('C++');
   }

   /**
    * Compute the reference times. For each category, the reference times
    * are taken from the container whose 'base' attribute is equal to base
    */
   Reftime.prototype.set_reftimes = function(base) {
      angular.forEach(this.data.categories, function(cat, catname) {
         cat.reftimes = {};

         angular.forEach(cat.containers, function(container) {
            if (container.base == base) {
               angular.forEach(container.tests, function(test, name) {
                  cat.reftimes[name] = test.mean_duration;
               });
            }
         });
      });
   };

   /**
    * Return the computed percent value for a given test
    */
   Reftime.prototype.percent = function(test_name, category, mean_duration) {
      var ref = test_name_to_reftime[test_name] || test_name;
      var rt = this.data.categories[category].reftimes[ref];  //  ref time
      return mean_duration / rt;
   };

   return new Reftime;
}).

controller('ResultsCtrl', function($scope, Reftime) {
   $scope.data = Reftime.data;  // from global variable
   $scope.ref_test_names = ref_test_names;
}).

controller('HeaderCtrl', function(Reftime, $scope) {
   $scope.data = Reftime.data;
   $scope.percents = [
      {value: true, text: 'As percent'},
      {value: false, text: 'As milliseconds'}
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
            $scope.test.percent = (Reftime.percent(
               $scope.testname,
               $scope.container.category,
               $scope.test.mean_duration) * 100).toFixed(0);
            $scope.test.mean_duration_str = (
               $scope.test.mean_duration * 1000).toFixed(2);
         }
      },
   template: '<span ng-if="test"' +
           ' ng-class="{worse:test.percent>105, comment:test.comment}"' +
           ' title="{{test.comment}}">' +
         '<span ng-if="$root.as_percent">{{test.percent}}%</span>' +
         '<span ng-if="!$root.as_percent">{{test.mean_duration_str}}</span>' +
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
