#Recommended Ruby version: 1.9.3 or higher

VERSION = 254 #legacy variable

#parameters and constants
scompltype="w" #measure lexicogrammatical diversity as TTR and not number of word forms or sentences
@splitcomplexnouns = true #treat dummy verbs (see article) as independent words
@cleantypos = true #clean obvious typos like an extra slash in language 2 and extra spaces

measures_to_output = ["ttr","ttr_nouns","ttr_verbs","expr_verb_gender","expr_noun_number","expr_verb_lex","expr_noun_lex","comprehension_rate","underspecification","change_rate","dexpr_noun_lex","dexpr_verb_lex","dttr","dexpr_verb_gender","dexpr_noun_number"] #expr = expressibility; gender = agreement; noun_lex = agent (the meaning of the nominal stem); verb_lex = event (the meaning of the verbal stem); d = delta, see section 4.5

CONTACT_LENGTH = 3 #generations 2, 3, 4
CHAIN_LENGTH = 10 
FULLMSPACE = ["1","2","1 a","1 b","1 c","2 a","2 b","2 c","1s","2s","1s a","1s b","1s c","2s a","2s b","2s c"] #meaning space

rough = File.open("Languages/rough_data2.csv","r:utf-8") #The file with the necessary data about participants must be available

#define variables
alldata = [[],[],[],[],[]] #chain, generation, chain-type, learner type (l1, l2, post-l2:n), w
levenshtein_norm = []
affected_forms_norm = [] #a change rate not used in the article: number of forms changed
sent_ambiguity = [] 
sent_ambiguity_norm = []
levenshtein10_norm = [] #for storing distances between last and 0 languages 
affected_forms10_norm = []
speakercomplexity_norm = []
ttr_n = []
ttr_v = []
languages = [] 
socdata = [[],[]] #gender, age
expr_verb_gender = []
expr_verb_number = []
expr_noun_number = []
expr_noun_lex = []
expr_verb_lex = []


def longest_common_substr(strings) #http://stackoverflow.com/questions/2158313/finding-common-string-in-array-of-strings-ruby
  shortest = strings.min_by &:length
  maxlen = shortest.length
  maxlen.downto(0) do |len|
    0.upto(maxlen - len) do |start|
      substr = shortest[start,len]
      return substr if strings.all?{|str| str.include? substr }
    end
  end
end

def levenshtein_distance(s, t) #from the levenshtein gem
  m = s.length
  n = t.length
  return m if n == 0
  return n if m == 0
  d = Array.new(m+1) {Array.new(n+1)}

  (0..m).each {|i| d[i][0] = i}
  (0..n).each {|j| d[0][j] = j}
  (1..n).each do |j|
    (1..m).each do |i|
      d[i][j] = if s[i-1] == t[j-1]  # adjust index into string
                  d[i-1][j-1]       # no operation required
                else
                  [ d[i-1][j]+1,    # deletion
                    d[i][j-1]+1,    # insertion
                    d[i-1][j-1]+1,  # substitution
                  ].min
                end
    end
  end
  d[m][n]
end

def language_to_wordforms(language)
  wordforms = []
  language.each_index do |phraseind| #"phrase" is used here and elsewhere as a synonym to "sentence"
	phrase = language[phraseind]
   
	if phrase
	  phrase = phrase.strip
	  if [0,1,8,9].include?(phraseind) and !@splitcomplexnouns
	    wordforms << phrase
	  else
	    phrase.split(" ").each do |word|
	      wordforms << word
	    end
	  end
	end
  end
  return wordforms
end

def language_to_wordforms_pos(language)
  nouns = []
  verbs = []
  language.each_index do |phraseind|
    phrase = language[phraseind]
	if phrase
	  if [0,1,8,9].include?(phraseind) and !@splitcomplexnouns
	    nouns << phrase
	  else
		phrase1 = phrase.strip.split(" ")
	    nouns << phrase1[0]
	    if phrase1[1]
	      verbs << phrase1[1]
	    end
	  end
	end
  end
  return [nouns,verbs]
end

def readlineno(filename,exactlinenumber)
  file=File.open(filename,"r")
  j = 0
  res = ""
  file.each_line do |fileline|
    if j == exactlinenumber
	  res = fileline.strip
	  break
    end
    j = j+1
  end
  file.close
  return res  
end

def ambiguity(language)
  return (language.length-language.uniq.length)
end

def calculate_distances_norm(language_new,language_old)
  totalforms = 0.0
  distance = 0.0
  nforms = 0.0
  language_new.each_index do |index|
    if language_new[index] and language_old[index]
	  totalforms += 1.0
      distance = distance + levenshtein_distance(language_new[index],language_old[index]).to_f/[language_new[index].length,language_old[index].length].max
      if levenshtein_distance(language_new[index],language_old[index])>0
	    nforms = nforms + 1.0
	  end
	end
  end
  return [distance/totalforms, nforms/totalforms]  
end

def speakercomplexity1_norm(language,wordforms, scompltype) #normalizing
  scomplexity1 = language.uniq.length.to_f/language.compact.length #normalizing by the total number of sentences 
  scomplexity2 = wordforms.uniq.length.to_f/wordforms.length #...and the total number of wordforms (in fact that's a TTR)
  if scompltype == "j"
    scomplexity4 = harmonic_mean(scomplexity1,scomplexity2)
  elsif scompltype == "s"
    scomplexity4 = scomplexity1
  elsif scompltype == "w"
    scomplexity4 = scomplexity2
  end
  
  return scomplexity4
end


def all_pos(language,pos) #Pick up all nouns or all verbs
  outcome = []
  
  posvindices = [2,3,4,5,6,7,10,11,12,13,14,15]
  posnindices = (0..15).to_a
  existindices = [0,1,8,9]
    
  if pos == "V"
    indices = posvindices
	index = 1
  elsif pos == "N"
    indices = posnindices
	index = 0
  end
  
  indices.each do |ind|
    if language[ind]
      phrase = language[ind].split(" ")
	  if existindices.include?(ind)
   	    if phrase.length == 1 #or !@nosecondparts
		  outcome << language[ind]
        elsif phrase.length == 2 #and @nosecondparts
		  outcome << phrase[0]
		else
		  STDERR.puts "Can't split a phrase!"
		end

	  else
	    if phrase.length == 1 or phrase.length == 2
	      outcome << phrase[index].to_s
	    else
	      STDERR.puts "Can't split a phrase!"
	    end  
	  end  
	end
  end
  return outcome
end

def cluster_pos(forms,pos,category) #split a given part of speech across a given category for calculating partial measures
  
  nnindices1 = (0..7)
  nnindices2 = (8..15)
  nlindices1 = [0,2,3,4,8,10,11,12]
  nlindices2 = [1,5,6,7,9,13,14,15]
  vnindices1 = (0..5)
  vnindices2 = (6..11)
  vgindices1 = [0,1,2,6,7,8]
  vgindices2 = [3,4,5,9,10,11]
  vlindices1 = [0,3,6,9]
  vlindices2 = [1,4,7,10]
  vlindices3 = [2,5,8,11]
  
  distr1 = []
  distr2 = []
  distr1n = []
  distr2n = []
  
  if pos=="N"
    if category=="number"
      for n in nnindices1
	    distr1 << forms[n] #sg
	    distr1n << n
	  end
	  for n in nnindices2
	    distr2 << forms[n] #pl
	    distr2n << n
	  end
	elsif category=="lexical"
	  for n in nlindices1
	    distr1 << forms[n] #sg
	    distr1n << n
	  end
	  for n in nlindices2
	    distr2 << forms[n] #pl
	    distr2n << n
	  end
	end
  elsif pos == "V"
    if category=="number"
	  for n in vnindices1
	    distr1 << forms[n] #sg
		distr1n << n
	  end
	  for n in vnindices2
	    distr2 << forms[n] #pl
		distr2n << n
	  end
	elsif category=="gender"
	  for n in vgindices1 
	    distr1 << forms[n] #gender1
		distr1n << n
	  end
	  for n in vgindices2
	    distr2 << forms[n] #gender2
		distr2n << n
	  end
	elsif category=="lexical"
      
	  for n in vlindices1
	    distr1 << forms[n] #verb 1
		distr1n << n
	  end
	  for n in vlindices2
	    distr2 << forms[n] #verb 2
		distr2n << n
	  end
	  	    
	  distr3 = []
      distr3n = []
	  for n in vlindices3
	    distr3 << forms[n] #verb 3
		distr3n << n
	  end
	  
	end
  end
  if pos=="V" and category=="lexical" 
    return [[distr1,distr2,distr3],[distr1n,distr2n,distr3n]]
  else
    return [[distr1,distr2],[distr1n,distr2n]] # the second element of the array is not in use now.
  end	
end

def pair_expressivity(dualcluster) 
  expr = 0.0
  affixes = Hash.new(0.0)
  n = dualcluster[0].length
  
  dualcluster[0].each_index do |ind|
    if dualcluster[0][ind]!=dualcluster[1][ind]
	  expr +=1.0
	end  
  end
  expr2 = expr/n #relative expressivity
	  
  return expr2 
end

def calculate_partials(language)
  
  enouns = all_pos(language,"N")
  everbs = all_pos(language,"V")
  
  everbs_gender_cluster =  cluster_pos(everbs,"V","gender")
  everbs_number_cluster =  cluster_pos(everbs,"V","number")
  enouns_number_cluster =  cluster_pos(enouns,"N","number")
  enouns_lexical_cluster = cluster_pos(enouns,"N","lexical")
  everbs_lexical_cluster = cluster_pos(everbs,"V","lexical")
 
  verb_gender = pair_expressivity(everbs_gender_cluster[0])
  verb_number = pair_expressivity(everbs_number_cluster[0])
  noun_number = pair_expressivity(enouns_number_cluster[0]) #not reported in the article
  noun_lexical = pair_expressivity(enouns_lexical_cluster[0])
  
  verb_lexical1 = pair_expressivity(everbs_lexical_cluster[0][0..1])
  verb_lexical2 = pair_expressivity(everbs_lexical_cluster[0][1..2])
  verb_lexical3 = pair_expressivity([everbs_lexical_cluster[0][0],everbs_lexical_cluster[0][2]])
  verb_lexical = (verb_lexical1+verb_lexical2+verb_lexical3)/3
    
  return [verb_gender,verb_number,noun_number,noun_lexical,verb_lexical]
end


i = 0
fmeasures_to_output = {}

#The folder "Analyzed" has to be created in advance
measures_to_output.each do |measure|
  fmeasures_to_output[measure] = {}
  fmeasures_to_output[measure]["n"] = File.open("Analyzed/v#{VERSION}_#{measure}_n.csv", "w") #normal
  fmeasures_to_output[measure]["i"] = File.open("Analyzed/v#{VERSION}_#{measure}_i.csv", "w") #temp. interrupted
  fmeasures_to_output[measure]["d"] = File.open("Analyzed/v#{VERSION}_#{measure}_d.csv", "w") #perm. interrupted
  fmeasures_to_output[measure].each_value do |mfile|
    mfile.puts "chain;generation;mvalue" #mvalue = value of the given measure at the given generation of the given chain; prev_value = previous value, stored only for deltas
  end
end

#MAIN CYCLE. Going through the rough_data file, filling in the data about each participant, reading in and processing the respective language
rough.each_line do |line|
  line1 = line.strip
	
  if i > 0
    
    vmeasures_to_output = {}
    line2 = line1.split(";")
	alldata[0][i] = line2[0].to_i #chain
	alldata[1][i] = line2[1].to_i #generation
	STDERR.puts "i=#{i},chain=#{alldata[0][i]},gen=#{alldata[1][i]}"
	
	if line2[2]=="l0" #chain type and learner type
	  alldata[2][i] = "normal"
	  alldata[3][i] = "l1"
	elsif line2[2]=="l2"
	  alldata[2][i] = "temp_interrupted"
	  contact_starts_at = line2[2][1].to_i
	  contact_ends_at = line2[2][1].to_i + (CONTACT_LENGTH-1)
	  if alldata[1][i] < contact_starts_at
	    alldata[3][i] = "l1"
	  elsif alldata[1][i] >= contact_starts_at and alldata[1][i] <= contact_ends_at 
	    alldata[3][i] = "l2"
	  elsif alldata[1][i] > contact_ends_at
	    alldata[3][i] = "post-contact l1: #{alldata[1][i]-contact_ends_at}" #number of generations after contact
	  end
	else #delta
	  alldata[2][i] = "perm_interrupted"
	  if alldata[1][i] < 2
	    alldata[3][i] = "l1"
	  else
	    alldata[3][i] = "l2"
	  end
	end  

	if alldata[1][i]!=0
	  alldata[4][i] = line2[4].to_i #w, comprehension test result
	  socdata[0][i] = line2[6] #gender
	  socdata[1][i] = line2[7].to_i #age
	end

	#make sure this folder is available
	langfilename = "Languages/lang_#{alldata[0][i]}.csv"
	languages[i] = readlineno(langfilename,alldata[1][i]).split(",")
			
	if @cleantypos
	  languages[i].each_index do |phraseind|
	    phrase = languages[i][phraseind]
	    phrase.strip
	    phrase.gsub!("/","")
	    languages[i][phraseind]=phrase
	  end
	end
		
	vmeasures_to_output["comprehension_rate"]=(alldata[4][i].to_f/languages[i].length).round(3)
  
#Ambiguity (share of ambiguous signals)
    sent_ambiguity[i]=ambiguity(languages[i])
	sent_ambiguity_norm[i]=sent_ambiguity[i].to_f/languages[i].length.to_f
	vmeasures_to_output["underspecification"]=sent_ambiguity_norm[i]
		
#Partial measures (=expressibility measures for different categories)
	partial_values = calculate_partials(languages[i])
	expr_verb_gender[i] = partial_values[0]
	vmeasures_to_output["expr_verb_gender"] = expr_verb_gender[i]
	expr_verb_number[i] = partial_values[1]
	vmeasures_to_output["expr_verb_number"] = expr_verb_number[i]
	expr_noun_number[i] = partial_values[2]
	vmeasures_to_output["expr_noun_number"] = expr_noun_number[i]
	expr_noun_lex[i] = partial_values[3]
	vmeasures_to_output["expr_noun_lex"] = expr_noun_lex[i]
	expr_verb_lex[i] = partial_values[4]
	vmeasures_to_output["expr_verb_lex"] = expr_verb_lex[i]

#ttr 	
    wordforms = language_to_wordforms(languages[i])
	wpos = language_to_wordforms_pos(languages[i])
	speakercomplexity_norm[i] = speakercomplexity1_norm(languages[i],wordforms,scompltype)
	vmeasures_to_output["ttr"] = speakercomplexity_norm[i]
	ttr_n[i] = speakercomplexity1_norm(languages[i],wpos[0],scompltype)
	ttr_v[i] = speakercomplexity1_norm(languages[i],wpos[1],scompltype)
	vmeasures_to_output["ttr_nouns"] = ttr_n[i]
	vmeasures_to_output["ttr_verbs"] = ttr_v[i]

#measuring deltas (see section 4.5)	
	if alldata[1][i]!=0 
	  vmeasures_to_output["dttr"] = speakercomplexity_norm[i]-speakercomplexity_norm[i-1] #not discussed in the article
	  vmeasures_to_output["dexpr_verb_gender"] = expr_verb_gender[i]-expr_verb_gender[i-1]
	  vmeasures_to_output["dexpr_noun_number"] = expr_noun_number[i]-expr_noun_number[i-1]
	  vmeasures_to_output["dexpr_verb_lex"] = expr_verb_lex[i]-expr_verb_lex[i-1]
	  vmeasures_to_output["dexpr_noun_lex"] = expr_noun_lex[i]-expr_noun_lex[i-1]
	end

#distances
	if alldata[1][i]!=0
	  distances = calculate_distances_norm(languages[i],languages[i-1])
	  levenshtein_norm[i]=distances[0]
	  #affected_forms_norm[i]=distances[1] #how many forms were changed (ignoring the scale of change within each form). Not mentioned in the article
	  if alldata[1][i]==CHAIN_LENGTH #distances between language 10 and language 0
	    distances = calculate_distances_norm(languages[i],languages[i-10])
	    levenshtein10_norm[i]=distances[0]
        affected_forms10_norm[i]=distances[1]
	  end
    end
	vmeasures_to_output["change_rate"]=levenshtein_norm[i]
	vmeasures_to_output["0change_rate"]=levenshtein10_norm[i]

#data output
   fmeasures_to_output.each_pair do |measure,filearray|
      if vmeasures_to_output[measure]
	    if alldata[2][i] == "normal"  
          filearray["n"].puts "#{alldata[0][i]};#{alldata[1][i]};#{vmeasures_to_output[measure]}"
        elsif alldata[2][i] == "temp_interrupted" 
	      filearray["i"].puts "#{alldata[0][i]};#{alldata[1][i]};#{vmeasures_to_output[measure]}"
        elsif alldata[2][i] == "perm_interrupted" 
	      filearray["d"].puts "#{alldata[0][i]};#{alldata[1][i]};#{vmeasures_to_output[measure]}"
        end
	  end
    end
		
  end #MAIN IF end
  
  i += 1
end #MAIN CYCLE end

#closing output files
fmeasures_to_output.each_value do |measure|
  measure.each_value do |mfile|
    mfile.close
  end
end