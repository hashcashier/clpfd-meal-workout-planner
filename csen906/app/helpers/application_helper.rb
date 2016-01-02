module ApplicationHelper

  PROLOG_COMMAND = "prolog"

  SCRIPT_DIR = './scripts/'
  EXERCISE_FILE = 'exercise.pl'

  def self.solve_prolog(consult_script, query, print)
    script_file = SCRIPT_DIR + consult_script
    writelns = print.map { |x| "writeln(" + x + ")" }.join(",")
    `#{PROLOG_COMMAND} -s #{script_file} -g '#{query}, #{writelns}' -t 'halt'`
  end

end
