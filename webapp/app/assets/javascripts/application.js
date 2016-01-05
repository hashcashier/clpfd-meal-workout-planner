// This is a manifest file that'll be compiled into application.js, which will include all the files
// listed below.
//
// Any JavaScript/Coffee file within this directory, lib/assets/javascripts, vendor/assets/javascripts,
// or any plugin's vendor/assets/javascripts directory can be referenced here using a relative path.
//
// It's not advisable to add code directly here, but if you do, it'll appear at the bottom of the
// compiled file.
//
// Read Sprockets README (https://github.com/rails/sprockets#sprockets-directives) for details
// about supported directives.
//
//= require jquery
//= require jquery_ujs
//= require bootstrap-sprockets
//= require turbolinks
//= require_tree .


function parsePrologArray(data) {
  var ret = [];
  var depth = 0;
  var str = ""
  for(var i=1;i<data.length-1;i++){
    if(data[i] == '[' || data[i] == '(')
      depth++;
    else if(data[i] == ']' || data[i] == ')')
      depth--;
    if(data[i] == ',' && depth == 0){
      ret.push(str);
      str = "";
    }else{
      str += data[i];
    }
  }
  ret.push(str)
  return ret;
}
