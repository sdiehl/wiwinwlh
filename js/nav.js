$(function () {

  // NAV

  var $sidebar = $('#sidebar-wrapper');
  var $toggle = $('#toggle-sidebar');

  $toggle.click(function () {
    $sidebar.toggleClass('expanded');
  });

  $sidebar.find('.sidebar-nav a').click(function () {
    $sidebar.removeClass('expanded');
  });

  // TABLES
  $('#main table').each(function () {
    var $table = $(this).wrap('<div class="overflow-table"></div>');
  });

});
