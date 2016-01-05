$("#exercise-submit").click(function(){
  var numOfDays = $("#exercise-num-days").val();
  var type = $("#exercise-type").val();
  $("#loading").show();
  $.get("/exercise/solve", { numOfDays: numOfDays, type: type }, function(data){
      $("#loading").hide();
      parseAndShowExercise(data);
    }
  )
});

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

function parseExerciseRep(data) {
  var re = /^workout_rep\(([^,]*),(\d*)\)$/g;
  var matches = re.exec(data);

  return {
    exercise : matches[1],
    reps : parseInt(matches[2])
  };
}

function parseExerciseSet(data){
  var re = /^times\((\d*),(.*)\)$/g;
  var matches = re.exec(data);

  exercises = parsePrologArray(matches[2]).map(parseExerciseRep);

  return {
    exercises : exercises,
    times : parseInt(matches[1])
  };

}
function parseExerciseDay(data){
  var re = /^day\((\d*),([^,]*),(.*)\)$/g;
  var matches = re.exec(data);

  return {
    dayNum : parseInt(matches[1]),
    muscle : matches[2],
    sets : parsePrologArray(matches[3]).map(parseExerciseSet)
  }
}


function parseAndShowExercise(data) {
  // Trim new lines
  data = data.replace(/(\r\n|\n|\r)/gm,"");
  var weeks = parsePrologArray(data);
  var days = [];
  for(var i=0;i<weeks.length;i++){
    var week = weeks[i];
    days.push(parsePrologArray(week).map(parseExerciseDay));
  }

  var maxDay = 0;
  for(var i=0;i<days.length;i++)
    for(var j=0;j<days[i].length;j++)
      maxDay = Math.max(maxDay, days[i][j].dayNum);

  var pagination = $("#pagination");
  pagination.empty();
  $("#exercises").empty();


  var ids = 0;
  for(var i=0;i<4;i++){
    for(var j=0;j<maxDay;j++){
      var newLink = $("<a href='#' id='exercise-id-" + ids + "' > Week " + (i+1) + " Day " + (j+1) + " </a>");
      newLink.click(function(){
        var id = $(this).attr("id");
        id = id.substring(id.lastIndexOf("-")+1);
        $("#exercises > *").hide();
        $("#exercise-data-" + id).show();
        $("*[id^=\"exercise-id-\"]").each(function(){$(this).parent().removeClass("active");});
        $("#exercise-id-" + id).parent().addClass("active");
      });
      pagination.append($("<li></li>").append(newLink));
      ids++;
    }
  }

  var ids = 0;
  for(var i=0;i<4;i++){
    for(var j=0;j<maxDay;j++){
      var dayContent = [];
      for(var k=0;k<days[i].length;k++){
        if(days[i][k].dayNum == (j+1))
          dayContent.push(days[i][k]);
      }
      var newDev = $("<div style='display:none;' id='exercise-data-" + ids + "'></div>").append(renderDay(dayContent));
      $("#exercises").append(newDev);
      ids++;
    }
  }

}

function renderDay(data){
  var table = $("<table class='table'></table");
  for(var i=0; i<data.length;i++){
    var newMuscle = table.append($("<tr><td align='center' colspan='2'> Muscle " + data[i].muscle + "</td></tr>"));
    for(var j=0; j<data[i].sets.length;j++){
      var newSet = $("<tr></tr>");
      newSet.append($("<td class='col-md-6' align='center' >" + data[i].sets[j].times +"x</td>"));
      newSet.append(renderSet(data[i].sets[j].exercises));
      table.append(newSet);
    }
  }
  return table;
}

function renderSet(data){
  var ret = $("<table class='table'></table>");
  for(var i=0;i<data.length;i++){
    ret.append($("<tr><td>" + data[i].reps + "x " + data[i].exercise + "</td></tr>"));
  }
  return ret;
}
