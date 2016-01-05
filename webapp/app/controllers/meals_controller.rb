class MealsController < ApplicationController
  def index
  end

  def solve
    goal = params[:goal]
    mealsPerDay = params[:mealsPerDay]
    weight = params[:weight]
    fatPercentage = params[:fatPercentage]
    activityVariable = params[:activityVariable]
    query = "plan(#{[goal, mealsPerDay, weight, fatPercentage, activityVariable, Random.rand(1000), "Plan"].join "," })"
    plan = ApplicationHelper.solve_prolog("meals.pl", query, ["Plan"])
    render text: "#{plan}"
  end
end
