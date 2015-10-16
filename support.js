var app = angular.module('containers', []);

var test_name_to_reftime = {
   'fill': 'fill',
   'copy': 'fill',
   'cursor loop': 'cursor loop',
   'for-of loop': 'cursor loop',
   'count_if': 'cursor loop'
};

var all_test_names = [
   'fill',
   'copy',
   'cursor loop',
   'for-of loop',
   'count_if'
];

// Tests before which we display a bold border
var ref_test_names = {
   'fill': true,
   'cursor loop': true
};

// Same as data (from data.js), but grouped by element type
var processed_data = {};

app.factory('Reftime', function() {
   var reftimes = {};  // elem_type -> { testname -> duration }

   /**
    * This class is used to compute percents compared to various reference
    * times.
    */
   function Reftime() {
      this.set_reftimes('C++');
   }

   /**
    * Compute the reference times
    */
   Reftime.prototype.set_reftimes = function(base) {
      // Group data by element type
      angular.forEach(data.tests, function(container) {
         var arr = processed_data[container.elem_type];
         if (!arr) {
            arr = processed_data[container.elem_type] = [];
         }
         arr.push(container);
      });

      // Compute the reference times (C++ always for now)
      angular.forEach(data.tests, function(container) {
         if (container.base == base) {
            angular.forEach(container.tests, function(test, name) {
               if (!reftimes[container.elem_type]) {
                  reftimes[container.elem_type] = {};
               }
               reftimes[container.elem_type][name] = test.duration;
            });
         }
      });
   };

   /**
    * Return the computed percent value for a given test
    */
   Reftime.prototype.percent = function(test_name, elem_type, duration) {
      var r = test_name_to_reftime[test_name] || test_name;
      var rt = reftimes[elem_type];
      if (!rt) {
         rt = reftimes[elem_type] = {};
      }
      if (!rt[r]) {
         rt[r] = duration;
      }
      return duration / rt[r];
   };

   /**
    * Return the list of all known tests
    */
   Reftime.prototype.test_names = function() {
      return all_test_names;
   };

   return new Reftime;
});

app.controller('ResultsCtrl', function($scope, Reftime) {
   $scope.data = processed_data;  // from global variable
   $scope.as_percent = true;
   $scope.test_names = Reftime.test_names();
   $scope.ref_test_names = ref_test_names;
}).

directive('ctDuration', function() {
   return {
      scope: {
         testname: '=ctDuration',
         aspercent: '=',
         container: '='
      },
      controller: function($scope, Reftime) {
         $scope.test = $scope.container.tests[$scope.testname];
         if ($scope.test) {
            $scope.test.percent = (Reftime.percent(
                   $scope.testname,
                   $scope.container.elem_type,
                   $scope.test.duration) * 100).
                    toFixed(0);
            $scope.test.duration_str = ($scope.test.duration * 1000).toFixed(2);
         }
      },
   template: '<span ng-if="test"' +
           ' ng-class="{worse:test.percent>100, comment:test.comment}"' +
           ' title="{{test.comment}}">' +
         '<span ng-if="aspercent">{{test.percent}}%</span>' +
         '<span ng-if="!aspercent">{{test.duration_str}}</span>' +
         '</span>'
   };
}).

filter('kb', function() {
   return function(value) {
      if (!value) {
         return '';
      } else if (value > 1000) {
         return (value / 1000).toFixed(0) + 'kb';
      } else {
         return value + 'b';
      }
   };
});
