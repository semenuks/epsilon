#Recommended Ruby version: 1.9.3 or higher

VERSION = 336 #legacy variable

scompltype="w" #measure lexicogrammatical diversity as TTR and not number of word forms or sentences
@splitcomplexnouns = true #treat cases like "rub io" (P39-10) meaning 'round' animal as the noun "rub" and the dummy verb "io" and not as the noun "rub io"
@cleantypos = true #clean obvious typos like an extra slash in language 2 and extra spaces

measures_to_output = ["dexpr_verb_gender","dexpr_noun_number","dexpr_verb_lex","dexpr_noun_lex","dttr","comprehension_rate", "ttr", "expr_verb_gender","expr_noun_number","expr_verb_lex","expr_noun_lex","change_rate"] #expr = expressibility; gender = agreement; noun_lex = agent (the meaning of the nominal stem); verb_lex = event (the meaning of the verbal stem); d = delta, see section 4.5

rough = File.open("Languages/rough_data2.csv","r:utf-8") #The file with the necessary data about participants must be available
alldata = [[],[],[],[],[]] #chain, generation, chain-type, learner type (l1, l2, post-l2:n), comprehension score
levenshtein_norm = []
speakercomplexity_norm = []
dttr = []
comprehension_rate = []

languages = [] 
socdata = [[],[]] #gender, age
expr_verb_gender = []
expr_verb_number = []
expr_noun_number = []
expr_noun_lex = []
expr_verb_lex = []

CONTACT_LENGTH = 3 #generations 2, 3, 4

def div_by_zero(n1,n2)
  if n2==0
    d = 0.0
  elsif 
    d = n1/n2
  end
  return d
  
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

def pair_expressivity(type,dualcluster,main_i) #calculate expressivit for a given category
  expr = 0.0
  affixes = Hash.new(0.0)
  n = dualcluster[0].length
  
    dualcluster[0].each_index do |ind|
      if dualcluster[0][ind]!=dualcluster[1][ind]
	    expr +=1.0
	  end  
	  if type=="v"
		if dualcluster[0][ind].length > 1
		  affix1 = dualcluster[0][ind][0..-2]
		else
          affix1 = "" #assuming there are no single-consonant verbs. If there are, a more complicated rule has to be written
        end		  
		if dualcluster[1][ind].length > 1
		  affix2 = dualcluster[1][ind][0..-2]
		else
          affix2 = ""
        end
		affix = [affix1,affix2]
      elsif type=="vg" or type=="vn"
		if dualcluster[0][ind].length > 1
		  affix1 = dualcluster[0][ind][-1]
		else
          affix1 = dualcluster[0][ind]
        end		  
		if dualcluster[1][ind].length > 1
		  affix2 = dualcluster[1][ind][-1]
		else
          affix2 = dualcluster[1][ind]
        end
		affix = [affix1,affix2]
	  elsif type=="nn"
		affix = [dualcluster[0][ind][3].to_s,dualcluster[1][ind][3].to_s]
	  elsif type=="n"
		affix = [dualcluster[0][ind][0..2],dualcluster[1][ind][0..2]]   
	  end
	  if main_i==141 or main_i==142 or main_i==143
	    if type=="n"
		  affix = [dualcluster[0][ind][0..1],dualcluster[1][ind][1..3]]
		elsif type == "nn"
		  if dualcluster[0][ind][1]=="e"
		    affix=[dualcluster[0][ind][2..3],dualcluster[1][ind][2..3]]
		  elsif dualcluster[0][ind][1]=="i"
		    affix=[dualcluster[0][ind][0],dualcluster[1][ind][0]]
		  else 
            STDERR.puts "Error with chain 13"		  
		  end
		end
	  end
	  if affix[0]==affix[1] 
	    affix=["",""]
	  end
	  affixes[affix] += 1.0
    end
    	
    expr2 = expr/n
      
  return expr2
end

def calculate_expressivity(language,main_i)
  
  enouns = all_pos(language,"N")
  everbs = all_pos(language,"V")
  
  everbs_gender_cluster =  cluster_pos(everbs,"V","gender")
  everbs_number_cluster =  cluster_pos(everbs,"V","number")
  enouns_number_cluster =  cluster_pos(enouns,"N","number")
  enouns_lexical_cluster = cluster_pos(enouns,"N","lexical")
  everbs_lexical_cluster = cluster_pos(everbs,"V","lexical")
  
  expr_verb_gender = pair_expressivity("vg",everbs_gender_cluster[0],main_i)
  expr_verb_number = pair_expressivity("vn",everbs_number_cluster[0],main_i)
  expr_noun_number = pair_expressivity("nn",enouns_number_cluster[0],main_i)
  expr_noun_lexical = pair_expressivity("n",enouns_lexical_cluster[0],main_i)
  
  verb_lexical1 = pair_expressivity("v",everbs_lexical_cluster[0][0..1],main_i)
  verb_lexical2 = pair_expressivity("v",everbs_lexical_cluster[0][1..2],main_i)
  verb_lexical3 = pair_expressivity("v",[everbs_lexical_cluster[0][0],everbs_lexical_cluster[0][2]],main_i)
  
  expr_verb_lexical = (verb_lexical1+verb_lexical2+verb_lexical3)/3
  
  return [expr_verb_gender,expr_verb_number,expr_noun_number,expr_noun_lexical,expr_verb_lexical]
end

i = 0
previouslanguage = [] #stored for comparisons (Levenshtein etc.)

fmeasures_to_output = {}

measures_to_output.each do |measure|
  fmeasures_to_output[measure] = {}
  fmeasures_to_output[measure]["n"] = File.open("Analyzed/v#{VERSION}_#{measure}_n.csv", "w")
  fmeasures_to_output[measure]["i"] = File.open("Analyzed/v#{VERSION}_#{measure}_t.csv", "w")
  fmeasures_to_output[measure]["d"] = File.open("Analyzed/v#{VERSION}_#{measure}_p.csv", "w")
  fmeasures_to_output[measure].each_value do |mfile|
    mfile.puts "chain;generation;mvalue;prev_value;speaker"
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
	speakerid = "#{alldata[0][i]}-#{alldata[1][i]}"
	STDERR.puts "i=#{i},chain=#{alldata[0][i]},gen=#{alldata[1][i]}"
	
	if line2[2]=="l0" #chain type and learner type
	  alldata[2][i] = "normal"
	  alldata[3][i] = "l1"
	elsif line2[2]=="l2"
	  alldata[2][i] = "interrupted"
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
	  alldata[2][i] = "interrupted-2"
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

	langfilename = "Languages/lang_#{alldata[0][i]}.csv" #!!!e			
	languages[i] = readlineno(langfilename,alldata[1][i]).split(",")
	wordforms = language_to_wordforms(languages[i])
	if @cleantypos
	  languages[i].each_index do |phraseind|
	    phrase = languages[i][phraseind]
	    phrase.strip
	    phrase.gsub!("/","")
	    languages[i][phraseind]=phrase
	  end
	end
	
#comprehension rate
	comprehension_rate[i] = (alldata[4][i].to_f/languages[i].length).round(3)
	vmeasures_to_output["comprehension_rate"]=comprehension_rate[i]
  
#expressibility
	partial_values = calculate_expressivity(languages[i],i)
	
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
 	
#TTR
	speakercomplexity_norm[i] = speakercomplexity1_norm(languages[i],wordforms,scompltype)
	vmeasures_to_output["ttr"] = speakercomplexity_norm[i]

#dTTR	
	if alldata[1][i]!=0 
	  dttr[i] = speakercomplexity_norm[i]-speakercomplexity_norm[i-1]
	  vmeasures_to_output["dttr"] = "#{dttr[i]};#{speakercomplexity_norm[i-1]};#{speakerid}"
	end

#distances (=change rate, =transmission error)
	if alldata[1][i]!=0
	  distances = calculate_distances_norm(languages[i],previouslanguage)
	  levenshtein_norm[i]=distances[0]
    end
	vmeasures_to_output["change_rate"]=levenshtein_norm[i]
	
#data output
   fmeasures_to_output.each_pair do |measure,filearray|
      if vmeasures_to_output[measure]
	    if alldata[2][i] == "normal"  
          filearray["n"].puts "#{alldata[0][i]};#{alldata[1][i]};#{vmeasures_to_output[measure]}"
        elsif alldata[2][i] == "interrupted" 
	      filearray["i"].puts "#{alldata[0][i]};#{alldata[1][i]};#{vmeasures_to_output[measure]}"
        elsif alldata[2][i] == "interrupted-2" 
	      filearray["d"].puts "#{alldata[0][i]};#{alldata[1][i]};#{vmeasures_to_output[measure]}"
        end
	  end
    end
	
#saving previous language
	previouslanguage = languages[i]
  end #if i>0 end
  i =  i+1
end #MAIN CYCLE end

#closing output files
fmeasures_to_output.each_value do |measure|
  measure.each_value do |mfile|
    mfile.close
  end
end