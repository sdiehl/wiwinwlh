$(function () {
  var $sidebar = $('#sidebar-wrapper');
  var $toggle = $('#toggle-sidebar');

  $toggle.click(function () {
    $sidebar.toggleClass('expanded');
  });

  $sidebar.find('.sidebar-nav').click(function () {
    $sidebar.removeClass('expanded');
  });
});
