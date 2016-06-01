// FIXME add something here later
document.styleSheets[0].disabled = true;

$(function(){
  var $page_menu = $('#page-menu li');
  var $module_header = $('#module-header');
  $('#bootstrap-nav').empty().append($page_menu.clone());
  $('#package-header').addClass('container');
  $('#content').addClass('container');
  $('#footer').addClass('container footer');
  $module_header.append($('#description .doc'));
  $('.collapser').click(function(){
    $(this).toggleClass('active');
    var $el = $(this).next('.show,.hide,.togglable');
    if ($el.hasClass('hide')) {
      $el.show();
      $el.removeClass('hide');
      $el.addClass('togglable');
    }
    else if ($el.hasClass('show')) {
      $el.removeClass('show');
      $el.addClass('togglable');
    }
    else { $el.toggle(); }
  });
});
