# This script analyzes the languages (stored in the folder Languages) and outputs measurements to the folder Measures.

VERSION = 364 #Legacy variable.
@cleantypos = true #whether to remove symbols that are clearly the results of typos and downcase capitals. 
@splitcomplexnouns = true #whether "dummy verbs" (see S6) should be considered separate words

rough = File.open("Languages/rough_data2.csv","r:utf-8") #The file with the necessary data about participants must be available

#declaring arrays and hashes for storing variables
fidelity = []
alldata = [[],[],[],[],[]] #chain, generation, chain-type, learner type (l1, l2, post-l2:n), w
levenshtein_norm = []
sent_ambiguity = []
sent_ambiguity_norm = []
ttr_n = []
ttr_v = []
ttr_v_stem = []
ttr_v_affix = []
comprehension_rate = []
speakercomplexity_norm = []

languages = [] 
socdata = [[],[]] #gender, age
expr_verb_gender = []
expr_verb_number = []
expr_noun_number = []
expr_noun_lex = []
expr_verb_lex = []

CONTACT_LENGTH = 3 #generations 2, 3, 4

#defining methods
def ttr_within_verbs(language) # see 3.3
  delete_empty_strings = true
  markers_verb1 = []
  markers_verb2 = []
  markers_verb3 = []
  verbstems_noun1 = []
  verbstems_noun2 = []
  
language.each_index do |phraseind| #"phrase" is used here and elsewhere as a synonym to "sentence"
	phrase = language[phraseind]
    if phrase #if not nil (doesn't happen in practice!)
	  phrase = phrase.strip  
      verb = phrase.split(" ")[1].to_s
      if [2, 3, 4, 10, 11, 12].include?(phraseind) #first noun
        verbstems_noun1 << verb[0].to_s
      elsif [5, 6, 7, 13, 14, 15].include?(phraseind) #second noun
        verbstems_noun2 << verb[0].to_s
      end

      if [2, 5, 10, 13].include?(phraseind) #first verb
        markers_verb1 << verb[-1].to_s #taking the LAST symbol of the verb (empty string if there no verb)
      elsif [3, 6, 11, 14].include?(phraseind) #second verb
        markers_verb2 << verb[-1].to_s
      elsif [4, 7, 12, 15].include?(phraseind) #third verb
        markers_verb3 << verb[-1].to_s
      end
     
    else 
      STDERR.puts "Nil phrase!!!"
    end
  end
  if delete_empty_strings
    [verbstems_noun1, verbstems_noun2, markers_verb1, markers_verb2, markers_verb3].each do |array|
      array.delete("")
    end
  end

  ttr_marker_per_verb = (speakercomplexity1_norm(markers_verb1) + speakercomplexity1_norm(markers_verb2) + speakercomplexity1_norm(markers_verb3))/3.0
  ttr_stem_per_noun = (speakercomplexity1_norm(verbstems_noun1) + speakercomplexity1_norm(verbstems_noun2))/2.0
   
  return [ttr_marker_per_verb, ttr_stem_per_noun]
end

def language_to_wordforms(language)
  wordforms = []
  nouns = []
  verbs = []

  language.each_index do |phraseind| #"phrase" is used here and elsewhere as a synonym to "sentence"
	phrase = language[phraseind]
   
	if phrase #if not nil
	  phrase = phrase.strip
	  if [0,1,8,9].include?(phraseind) and !@splitcomplexnouns
	    wordforms << phrase
        nouns << phrase
	  else
        phrase1 = phrase.split(" ")
	    phrase1.each do |word|
	      wordforms << word
	    end
        nouns << phrase1[0]
	    if phrase1[1]
	      verbs << phrase1[1]
	    end
	  end
	end
  end
  return [wordforms, nouns, verbs]
end

def ambiguity(language)
  return (language.length-language.uniq.length)
end

def div_by_zero(n1,n2)
  if n2==0
    d = 0.0
  elsif 
    d = n1/n2
  end
  return d
  
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

def calculate_distances_norm(language_new,language_old)
  totalforms = 0.0
  distance = 0.0
  nforms = 0.0
  language_new.each_index do |index|
    if language_new[index] and language_old[index]
	  totalforms += 1.0
      distance = distance + div_by_zero(levenshtein_distance(language_new[index],language_old[index]).to_f,[language_new[index].length,language_old[index].length].max)
      if levenshtein_distance(language_new[index],language_old[index])>0
	    nforms = nforms + 1.0
	  end
	end
  end
  return [distance/totalforms, nforms/totalforms]  
end

def speakercomplexity1_norm(wordforms) #TTR
  scomplexity = div_by_zero(wordforms.uniq.length.to_f, wordforms.length) #...and the total number of wordforms (in fact that's a TTR)
  return scomplexity
end

def all_pos(language,pos) #Pick up all nouns or all verbs
  outcome = []
  if @nofallaparts
    posvindices = [2,3,4,5,8,9,10,11]
	posnindices = (0..11).to_a
	existindices = [0,1,6,7]
  else
    posvindices = [2,3,4,5,6,7,10,11,12,13,14,15]
    posnindices = (0..15).to_a
    existindices = [0,1,8,9]
  end
  
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
   	    if phrase.length == 1 or !@nosecondparts
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

def pair_expressivity(dualcluster,main_i) #calculate partial measures for a given category
  expr = 0.0
  n = dualcluster[0].length
  
    dualcluster[0].each_index do |ind|
    
	    if dualcluster[0][ind]!=dualcluster[1][ind]
	      expr +=1.0
	    end
	  
    end
	
    expr2 = expr/n
   
  return expr2
end

def calculate_partials(language,main_i)
  
  enouns = all_pos(language,"N")
  everbs = all_pos(language,"V")
  
  everbs_gender_cluster =  cluster_pos(everbs,"V","gender")
  everbs_number_cluster =  cluster_pos(everbs,"V","number")
  enouns_number_cluster =  cluster_pos(enouns,"N","number")
  enouns_lexical_cluster = cluster_pos(enouns,"N","lexical")
  everbs_lexical_cluster = cluster_pos(everbs,"V","lexical")
  
  expr_noun_lexical = pair_expressivity(enouns_lexical_cluster[0],main_i)
  expr_noun_number = pair_expressivity(enouns_number_cluster[0],main_i)
  
  verb_lexical1 = pair_expressivity(everbs_lexical_cluster[0][0..1],main_i)
  verb_lexical2 = pair_expressivity(everbs_lexical_cluster[0][1..2],main_i)
  verb_lexical3 = pair_expressivity([everbs_lexical_cluster[0][0],everbs_lexical_cluster[0][2]],main_i)

  expr_verb_lexical = (verb_lexical1+verb_lexical2+verb_lexical3)/3
    
  expr_verb_gender = pair_expressivity(everbs_gender_cluster[0],main_i)
  expr_verb_number = pair_expressivity(everbs_number_cluster[0],main_i)
    
  return [expr_verb_gender,expr_verb_number,expr_noun_number,expr_noun_lexical,expr_verb_lexical]
end

i = 0
previouslanguage = [] #stored for comparisons (Levenshtein etc.)

fmeasures_to_output = {}
measures_to_output = ["ttr","fidelity","ttr_verbs","ttr_nouns", "ttr_v_stem", "ttr_v_affix", "comprehension_rate", "underspecification", "expr_verb_gender", "expr_noun_number", "expr_noun_lex", "expr_verb_lex", "entropy"] #this line specifies for which measures the output files will be provided

measures_to_output.each do |measure| #creating necessary output files
  fmeasures_to_output[measure] = {}
  fmeasures_to_output[measure]["n"] = File.open("Measures/v#{VERSION}_#{measure}_n.csv", "w")
  fmeasures_to_output[measure]["i"] = File.open("Measures/v#{VERSION}_#{measure}_t.csv", "w")
  fmeasures_to_output[measure]["d"] = File.open("Measures/v#{VERSION}_#{measure}_p.csv", "w")
  fmeasures_to_output[measure].each_value do |mfile|
    mfile.puts "chain;generation;mvalue"
  end
end

def hashentropy(hash,total)
  entr = 0.0
  #normalizer=hash.keys.length
  #if normalizer > 1
  if total > 1  
	hash.each_value do |v|
      if v > 0
	    entr += v/total*Math.log2(v/total)
	  end
    end
  end
  entr = -entr#/Math.log2(normalizer) 
  #end
  return entr
end



#MAIN CYCLE through the generations
rough.each_line do |line| #reading from the file with the data about participants
  line1 = line.strip
	
  if i > 0
    
    vmeasures_to_output = {} #storing values for output
    line2 = line1.split(";")
	alldata[0][i] = line2[0].to_i #chain
	alldata[1][i] = line2[1].to_i #generation
	speakerid = "#{alldata[0][i]}-#{alldata[1][i]}"
	STDERR.puts "i=#{i},chain=#{alldata[0][i]},gen=#{alldata[1][i]}"

	if line2[2]=="l0" #chain type and learner type. L1 = normal learner, L2 = imperfect learner
	  alldata[2][i] = "normal"
	  alldata[3][i] = "l1"
	elsif line2[2]=="l2"
	  alldata[2][i] = "t_interrupted"
	  contact_starts_at = line2[2][1].to_i
	  contact_ends_at = line2[2][1].to_i + (CONTACT_LENGTH-1)
	  if alldata[1][i] < contact_starts_at
	    alldata[3][i] = "l1"
	  elsif alldata[1][i] >= contact_starts_at and alldata[1][i] <= contact_ends_at 
	    alldata[3][i] = "l2"
	  elsif alldata[1][i] > contact_ends_at
	    #alldata[3][i] = "post-contact l1: #{alldata[1][i]-contact_ends_at}" #number of generations after contact
		alldata[3][i] = "l1"
	  end
	else 
	  alldata[2][i] = "p_interrupted"
	  if alldata[1][i] < 2
	    alldata[3][i] = "l1"
	  else
	    alldata[3][i] = "l2"
	  end
	end  

	if alldata[1][i]!=0
	  alldata[4][i] = line2[4].to_i #comprehension test result
	  socdata[0][i] = line2[6] #gender
	  socdata[1][i] = line2[7].to_i #age
	end

	langfilename = "Languages/lang_#{alldata[0][i]}.csv" #find the language of this participant
	languages[i] = readlineno(langfilename,alldata[1][i]).split(",") #read the language

	if @cleantypos #removing symbols that are clearly the results of typos and downcasing
	  languages[i].each_index do |phraseind|
	    phrase = languages[i][phraseind]
	    phrase = phrase.strip
		phrase.downcase!
	    phrase.gsub!("/","")
		phrase.gsub!("]","")
	    languages[i][phraseind]=phrase
	  end
	end
	
	comprehension_rate[i] = (alldata[4][i].to_f/languages[i].length).round(3)
	vmeasures_to_output["comprehension_rate"]=comprehension_rate[i]
	wordforms = language_to_wordforms(languages[i])

    

    allwordforms = wordforms[0]
    nounwordforms = wordforms[1]
    verbwordforms = wordforms[2]

#Entropy
    entropyhash = Hash.new(0.0)
    total = 0.0
    allwordforms.each do |wordform|
        entropyhash[wordform] += 1
        total += 1
    end
    vmeasures_to_output["entropy"] = hashentropy(entropyhash, total)
	
#Underspecification (share of ambiguous signals)
    sent_ambiguity[i]=ambiguity(languages[i])
	sent_ambiguity_norm[i]=sent_ambiguity[i].to_f/languages[i].length.to_f
	vmeasures_to_output["underspecification"]= sent_ambiguity_norm[i]

#expressibility
    partial_values = calculate_partials(languages[i],i)
	expr_verb_gender[i] = partial_values[0]
	vmeasures_to_output["expr_verb_gender"] = expr_verb_gender[i]
	
	expr_verb_number[i] = partial_values[1] #verb number (not reported in the article and not included in the output by default) 
	vmeasures_to_output["expr_verb_number"] = expr_verb_number[i]
	
	expr_noun_number[i] = partial_values[2]
	vmeasures_to_output["expr_noun_number"] = expr_noun_number[i]
	expr_noun_lex[i] = partial_values[3]
	vmeasures_to_output["expr_noun_lex"] = expr_noun_lex[i]
	expr_verb_lex[i] = partial_values[4]
	vmeasures_to_output["expr_verb_lex"] = expr_verb_lex[i]
 	
#calculating TTR
	speakercomplexity_norm[i] = speakercomplexity1_norm(allwordforms)
	vmeasures_to_output["ttr"] = speakercomplexity_norm[i]
	
	#calculating TTR separately for nouns and verbs (See S6)
	
	ttr_n[i] = speakercomplexity1_norm(nounwordforms)
	ttr_v[i] = speakercomplexity1_norm(verbwordforms)
    
    ttr_within = ttr_within_verbs(languages[i])
    ttr_v_affix[i] = ttr_within[0]
    ttr_v_stem[i] = ttr_within[1]
	vmeasures_to_output["ttr_nouns"] = ttr_n[i]
	vmeasures_to_output["ttr_verbs"] = ttr_v[i]
    vmeasures_to_output["ttr_v_affix"] = ttr_v_affix[i]
    vmeasures_to_output["ttr_v_stem"] = ttr_v_stem[i]
	
#calculating transmission fidelity 
	if alldata[1][i]!=0
	  distances = calculate_distances_norm(languages[i],previouslanguage)
	  levenshtein_norm[i]=distances[0]
	  fidelity[i] = (1 - levenshtein_norm[i])
	end
	
	#vmeasures_to_output["change_rate"]= levenshtein_norm[i]
	vmeasures_to_output["fidelity"] = fidelity[i]
	 
#preparing data for the output
   fmeasures_to_output.each_pair do |measure,filearray|
      if vmeasures_to_output[measure]
	    if alldata[2][i] == "normal"  
          filearray["n"].puts "#{alldata[0][i]};#{alldata[1][i]};#{vmeasures_to_output[measure]}"
        elsif alldata[2][i] == "t_interrupted" 
	      filearray["i"].puts "#{alldata[0][i]};#{alldata[1][i]};#{vmeasures_to_output[measure]}"
        elsif alldata[2][i] == "p_interrupted" 
	      filearray["d"].puts "#{alldata[0][i]};#{alldata[1][i]};#{vmeasures_to_output[measure]}"
        end
	  end
    end

#saving previous language in order to measure distances 
    previouslanguage = languages[i]
  end
  i =  i+1
end #MAIN CYCLE end


numlangs = i-1

#data output
fmeasures_to_output.each_value do |measure|
  measure.each_value do |mfile|
    mfile.close
  end
end

