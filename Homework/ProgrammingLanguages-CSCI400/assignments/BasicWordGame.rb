require 'minitest/autorun'

=begin
This exercise practices basic Ruby syntax.
=end

class WordGame
=begin
  word_score calculates a simple score for a word, which is the 
  length of the word + the frequency of the letter repeated most 
  (e.g., score for hello is length (5) + frequency (2, for l))
=end
  def word_score(word)
    score = word.length
    score += count_letters(word)
    return score
  end

=begin
  count_letters returns the frequency of the letter repeated most often.
  For practice, you MUST:
  - use a hash table
  - use the .each syntax to iterate the hash after it's created (to determine
  the max)
  Hint: 
  - I used each_char to iterate over the word. This is not available in Ruby 1.8.x. 
=end
  def count_letters(word)
    letters = Hash.new
    word.each_char{|c| letters[c] = if letters[c] then letters[c] + 1 else 1 end}
    max = 0
    letters.each {|letter, freq| if freq > max then max = freq end}
    return max
  end

=begin
  leader_board displays the top 3 scores + remaining total (if > 3). See the
  screenshot in the writeup. 
  For practice, you MUST:
  - use a conditional return value to set Lone wolf vs Top Dog
  - use parallel assignment and a splat to extract the 3 top scores put the rest 
  in an array
  - use a .each iterator to calculate the sum of the remaining scores 
  - use unless as an expression modifier to control whether the remaining
  scores are displayed (not that the first 3 lines always show up, the remaining
  only shows up if there are >3 scores)
  - make use of display_score to actually display the scores
=end

  def leader_board(scores)
    canine = if scores.length > 1 then "Top Dog" else "Lone Wolf" end
    print "We have a ", canine, "\n"
    first, second, third, *remaining = scores
    display_score(first, "first")
    display_score(second, "second")
    display_score(third, "third")
    if remaining.length > 0
      tot = 0
      remaining.each {|a| tot += a}
    end
    print "Remaining score: #{tot}\n" unless !tot
    print "Congratulations to ", canine, "\n\n"
  end

=begin
  display_score either displays "The (which score) score is (value)" OR
  "There is no (which score) score". See the screenshot
  For practice you MUST:
  - use string interpolation
=end
  def display_score score, rank
    message = (if score && score > 0 then "The #{rank} score is #{score}" else "There is no #{rank} score" end)
    print "#{message}\n"
  end

=begin
  create_scores is used to create an array of scores of various lengths (specifically,
  of sizes from 1 to 8), that can be used to test the leader board and quartile displays.
  For simplicity, the first score added to the array will be 100. Each succeeding score
  will subtract 10, so the final scores array is [100,90,80,70,60,50,40,30].
  For practice you MUST:
  - use the until loop control. 
  - Notice that leader_board is called within your loop, so
  that you can ensure that it works correctly for 1, 2, 3, 4 and 5 scores.  You will 
  generate 8 scores, but to reduce clutter, you will NOT call leader_board if there are
  more than 5 scores. 
  - use an if expression modifier to control when leader_board is called
  - use << to add items to the array
=end

  def create_scores
    scores = Array.new
    score = 100
    until scores.length >= 8 do
      scores << score
      if scores.length < 6
        leader_board(scores)
      end
      score -= 10
    end
    return scores
  end

=begin
  get_quartiles and display_quartiles work together to display the totals within
  the four quartiles (i.e., [100,90] [80,70] [60,50] [40,30]). You should NOT assume
  that the array will always be of size 8.  But for simplicity you MAY assume that 
  the array size is evenly divisible by 4.  This is NOT a good assumption in general, 
  but is OK for our purposes, because the real focus is how to use yield.
  For practice you MUST:
  - use .each with a range (e.g., the indices to be included in a total can be 
  expressed as a range)
  - include a loop of your choice to calculate all four quartiles
  - when you have a quartile total, yield it to the caller
  - other bookkeeping (e.g., setting totals to 0, etc.) can be coded however you see fit.
=end
  def get_quartiles(scores)
    quart = 0
    (0..(scores.length/4 - 1)).each{|n| quart += scores[n]}
    yield quart
    quart = 0
    ((scores.length/4)..(scores.length/2 - 1)).each{|n| quart += scores[n]}
    yield quart
    quart = 0
    ((scores.length/2)..(3 * scores.length/4 - 1)).each{|n| quart += scores[n]}
    yield quart
    quart = 0
    ((3*scores.length/4)..(scores.length - 1)).each{|n| quart += scores[n]}
    yield quart
  end

=begin
display_quartiles just displays the quartile totals
For practice you MUST:
- use a block, have get_quartiles yield the totals to be printed
Hint:
- you can pass parameters to methods that contain a yield (not shown in slide)
=end  
  def display_quartiles(scores)
    get_quartiles(scores) {|quart| print "Quartile total: #{quart}\n"}
    print "\n"
  end

end

=begin
This is a scripting language, so we do not require a "main" per se. 
The following lines of code will get executed when this script runs. 
You should not modify these lines.
=end
game = WordGame.new
puts "Showing a couple of word scores"
puts "Score for hello is: #{game.word_score("hello")} (should be 7)"
puts "Score for banana is:  #{game.word_score("banana")} (should be 9)"
puts "\nShowing the leader board, various options" 
scores = game.create_scores
puts "\nShowing the quartiles"
game.display_quartiles scores
puts "\nUnit tests follow..."

=begin
Add a unit test to test the two word scores displayed above (hello and banana)
and at least 3 others.
=end
class TestWordGame < MiniTest::Unit::TestCase
  def setup
    @game = WordGame.new
  end

  def test_wordscores
    assert_equal 7, @game.word_score("hello")
    assert_equal 9, @game.word_score("banana")
  end

  def test_scoreslist
    scores = @game.create_scores
    assert_equal 8, scores.length
    score = 100
    scores.each {|s| assert_equal score, s; score-=10}
  end

  def test_quartiles
    scores = [0, 1, 2, 3]
    index = 0
    @game.get_quartiles(scores) {|quart| puts "index: #{index}, score: #{scores[index]}, quart: #{quart}"; assert_equal scores[index], quart; index = index + 1}
  end

  def test_countletters
    assert_equal 0, @game.count_letters("")
    assert_equal 2, @game.count_letters("Alfalfa")
  end
end