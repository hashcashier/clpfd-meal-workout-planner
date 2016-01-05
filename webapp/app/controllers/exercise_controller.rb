class ExerciseController < ApplicationController
  def index
  end

  def solve
    numOfDays = params[:numOfDays]
    type = params[:type]
    query = "exercise_plan_month(#{numOfDays}, #{type}, Plan)"
    plan = ApplicationHelper.solve_prolog("exercise.pl", query, ["Plan"])
    render text: "#{plan}"
  end
end
