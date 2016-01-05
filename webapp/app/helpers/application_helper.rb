module ApplicationHelper

  PROLOG_COMMAND = "prolog"

  SCRIPT_DIR = './scripts/'
  EXERCISE_FILE = 'exercise.pl'

  def self.solve_prolog(consult_script, query, print)
    script_file = SCRIPT_DIR + consult_script
    writelns = print.map { |x| "writeln(" + x + ")" }.join(",")

    full_query = [query, writelns].join(",").chomp ","
    puts full_query

    `#{PROLOG_COMMAND} -s #{script_file} -g '#{full_query}' -t 'halt'`
  end

end
