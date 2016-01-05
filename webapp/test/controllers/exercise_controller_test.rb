require 'test_helper'

class ExerciseControllerTest < ActionController::TestCase
  test "should get index" do
    get :index
    assert_response :success
  end

  test "should get solve" do
    get :solve
    assert_response :success
  end

end
