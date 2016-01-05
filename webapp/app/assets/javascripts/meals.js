$("#meals-submit").click(function(){
  var goal = $("#meals-goal").val();
  var mealsPerDay = $("#meals-num-meals").val();
  var weight = $("#meals-weight").val();
  var fatPercentage = $("#meals-fat-percentage").val();
  var activityVariable = $("#meals-activity-variable").val();
  $("#loading").show();
  $.get("/meals/solve", {goal:goal, mealsPerDay:mealsPerDay, weight:weight, fatPercentage:fatPercentage, activityVariable: activityVariable}, function(data){
      $("#loading").hide();
      parseAndShowMeals(data);
    }
  )
});

function parseMealItem(data) {
  var re = /^eat\((\d*),(.*)\)$/g;
  var matches = re.exec(data);

  return {
    item : matches[2],
    units : parseInt(matches[1])
  };
}

function parseMeal(data) {
  return {
    items : parsePrologArray(data).map(parseMealItem),
  };
}

function parseAndShowMeals(data) {
  // Trim new lines
  data = data.replace(/(\r\n|\n|\r)/gm,"");

  var daySchedules = [];
  var weekSchedules = parsePrologArray(data);
  for(var i=0;i<weekSchedules.length;i++){
    daySchedules = daySchedules.concat(parsePrologArray(weekSchedules[i]));
  }

  var parsedDaySchedules = [];
  for(var i=0;i<daySchedules.length;i++){
    var tmp = daySchedules[i];
    parsedDaySchedules.push({
      meals: parsePrologArray(tmp).map(parseMeal)
    });
  }

  var pagination = $("#pagination");
  pagination.empty();
  $("#meals").empty();

  for(var i=0;i<parsedDaySchedules.length;i++){
    var newLink = $("<a href='#' id='meal-id-" + i + "' >" + (i+1) + " </a>");
    newLink.click(function(){
      var id = $(this).attr("id");
      id = id.substring(id.lastIndexOf("-")+1);
      $("#meals > *").hide();
      $("#meal-data-" + id).show();
      $("*[id^=\"meal-id-\"]").each(function(){$(this).parent().removeClass("active");});
      $("#meal-show-all").parent().removeClass("active");
      $("#meal-id-" + id).parent().addClass("active");
    });
    pagination.append($("<li></li>").append(newLink));
  }

  var showAll = $("<a href='#' id='meal-show-all' >Show All</a>");
  showAll.click(function(){
      $("*[id^=\"meal-id-\"]").each(function(){$(this).parent().removeClass("active");});
      $("#meal-show-all").parent().addClass("active");
      $("#meals > *").show();
  });
  pagination.append($("<li></li>").append(showAll));

  for(var i=0;i<parsedDaySchedules.length;i++){
    var newDev = $("<div style='display:none;' id='meal-data-" + i + "'></div>");
    newDev.append($("<h1 class='text-center'>Day "+ (i+1) +"</h1>"))
    newDev.append(renderMealDay(parsedDaySchedules[i].meals));
    $("#meals").append(newDev);
  }

}

function renderMealDay(data){
  var table = $("<table class='table'></table");
  for(var i=0; i<data.length;i++){
    var newMuscle = table.append($("<tr><td align='center' colspan='2'> Meal " + (i+1) + "</td></tr>"));
    for(var j=0; j<data[i].items.length;j++){
      var newMeal = $("<tr></tr>");
      newMeal.append($("<td class='col-md-6' align='center' >" + data[i].items[j].units +"x</td>"));
      newMeal.append($("<td align='center' >" + data[i].items[j].item +"</td>"));
      table.append(newMeal);
    }
  }
  return table;
}
