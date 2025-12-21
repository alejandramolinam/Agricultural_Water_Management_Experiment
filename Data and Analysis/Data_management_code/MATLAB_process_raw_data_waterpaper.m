clear all
%%%%%%%%%%%%%%%%%%%%%%%%%%
% Path Raw Data
folder='../data/raw/';
files=dir([folder,'*.csv']); % There is one file for experimental session

% Path Processed Data
outfile1_csv = '../data/processed/Data.csv'; % Complete Dataset in CSV format
outfile2_csv = '../data/processed/Data_surveys.csv'; % Only surveys data in CSV format
outfile_structure_m = '../data/processed/Data_matlab_structure_format.mat'; % Complete Dataset in matlab format
outfile_table_m_rounds = '../data/processed/Data_matlab_table_format_rounds.mat'; % Complete Dataset in matlab format
outfile_table_m_questions = '../data/processed/Data_matlab_table_format_questions.mat'; % Complete Dataset in matlab format

% Auxiliar Variables
read_files = 1;
create_csv = 1;
remove_data= 1; % Remove participant that didn't finish the experiment 

% Classification of the sessions
filesAG=[5,6,7,9,10,11,14,15]; % sessions with Water Right Owners and Farmers
filesStudent=[1,2,3,12,13]; % sessions with Students

% Process raw file
if read_files
    BD=[];
    for fi=1:length(files)
        P=[];    
        T=readtable([folder,files(fi).name]);

        rp=2; %number of test rounds 
        data2024=0;
        
        if strcmp(T.session_config_name{1},'AGUA_CONTROL')
            pfsi = 'survey_water_ini'; %prefix initial survey
            pfse = 'survey_water_end'; %prefix final survey
            pfg = 'game_water_control_'; %prefix game            
        elseif strcmp(T.session_config_name{1},'AGUA_TRATAMIENTO')
            pfsi = 'survey_water_ini'; %prefix initial survey
            pfse = 'survey_water_end'; %prefix final survey
            pfg = 'game_water_treatment_'; %prefix game           
        else
            pfg='agricultores_postpilotaje_ctrl_';
            pfsi='survey_inicial';
            pfse='survey_final';            
            data2024 = 1;
            if isnan(T.([pfg,num2str(1),'_player_tratamiento'])(1))                
                pfg='agricultores_postpilotaje_tto_';
            end
        end
        
        for i=1:size(T,1)    

            % General information of the participant  
            if ismember(fi,filesAG)
                P(i).WUA              = 1;       % Create variable to classify participant as a WUA or Farmer 
            else
                P(i).WUA              = 0;       % Create variable to classify participant NOT as a WUA or Farmer
            end
            if ismember(fi,filesStudent)
                P(i).student              = 1;       % Create variable to classify participant as a student or not
            else
                P(i).student              = 0;       % Create variable to classify participant NOT as a student or not
            end
            P(i).session              = fi; %ID Sesión
            P(i).file                 = {files(fi).name}; %Raw data file name            
            P(i).treatment            = T.([pfg,num2str(1),'_player_tratamiento'])(i); % Treatment (0:Control-1:treated)
            P(i).player               = T.('participant_code')(i); % ID Player    
                        
            % Data Initial Survey            
            P(i).age                  = 2025-T.([pfsi,'_1_player_edad'])(i);
            if P(i).age <=50  % Age categorization into two groups: under 50 or over 50
                P(i).age_cat          =1; % under or equal 50
            else
                P(i).age_cat          =2; % over 50
            end
            if data2024
                P(i).education_level      = T.([pfsi,'_1_player_escolaridad'])(i);
            else
                P(i).education_level      = T.([pfsi,'_1_player_escolaridadjefehogar'])(i);
            end
            if P(i).education_level<=3
                P(i).education_level_cat=1;
            else
                P(i).education_level_cat=2;
            end             
            if strcmp(T.([pfsi,'_1_player_genero'])(i),'Masculino')  % Gender (0: Male - 1:Female) 
                P(i).woman                 =0;
            else
                P(i).woman                 =1;
            end        
            P(i).risk_av              =T.([pfsi,'_1_player_risk_aversion'])(i); % Risk aversion question has 5 possible answers        
            if P(i).risk_av <=2    % risk_av_cat=3 : Answer 1 or 2 in the question, High Risk Aversion
                P(i).risk_av_cat      =2;            
            else                   % risk_av_cat=1 : Answer 3, 4 or 5 in the question, Low Risk Aversion 
                P(i).risk_av_cat      =1;
            end        
            P(i).beliefs_pre          =T.([pfsi,'_1_player_pre_beliefs_mas'])(i); % Beliefs before the game

            % Data Intermediate Survey
            P(i).beliefs_inter        =T.([pfg,'7_player_pre_beliefs_mas'])(i);     % Beliefs after round 5 of the game

            % Data Final Survey
            P(i).beliefs_post       =T.([pfse,'_1_player_post_beliefs_mas'])(i); % Beliefs after the game   
            P(i).cause_autority     =T.([pfse,'_1_player_causa_autoridad'])(i); 
            P(i).cause_injustice    =T.([pfse,'_1_player_causa_injusticia'])(i);
            P(i).cause_file         =T.([pfse,'_1_player_causa_multa'])(i);
            P(i).cause_necesity     =T.([pfse,'_1_player_causa_necesidad'])(i);
            P(i).cause_other_people =T.([pfse,'_1_player_causa_otros'])(i);
            P(i).open_question      =T.([pfse,'_1_player_pregunta_abierta'])(i);

            % Create Belifs Categories: 
            % Beliefs about the percentage of people who will comply [1=0-30%, 2=31-60%,3 =61-100%] 
            P(i).beliefs_pre_cat=zeros(size(P(i).beliefs_pre));
            P(i).beliefs_pre_cat(P(i).beliefs_pre<=3)=1;
            P(i).beliefs_pre_cat(P(i).beliefs_pre>3 & P(i).beliefs_pre<=6)=2;
            P(i).beliefs_pre_cat(P(i).beliefs_pre>6)=3;
            P(i).beliefs_inter_cat=zeros(size(P(i).beliefs_inter));
            P(i).beliefs_inter_cat(P(i).beliefs_inter<=3)=1;
            P(i).beliefs_inter_cat(P(i).beliefs_inter>3 & P(i).beliefs_inter<=6)=2;
            P(i).beliefs_inter_cat(P(i).beliefs_inter>6)=3;
            P(i).beliefs_post_cat=zeros(size(P(i).beliefs_post));
            P(i).beliefs_post_cat(P(i).beliefs_post<=3)=1;
            P(i).beliefs_post_cat(P(i).beliefs_post>3 & P(i).beliefs_post<=6)=2;
            P(i).beliefs_post_cat(P(i).beliefs_post>6)=3;


            % Data from the game
            for r=(rp+1):17  % Rounds 2 to 17 (first 2 where for learning the game with no impact in payments)
                a=r-rp;

                % RAW VARIABLES
                P(i).round(a)          =a;
                P(i).turn(a)           =T.([pfg,num2str(r),'_player_turno'])(i);  %1: first turn- 2: second turn     
                P(i).demand(a)         =T.([pfg,num2str(r),'_player_demand'])(i); %Demand in that round in hours
                P(i).kept(a)           =T.([pfg,num2str(r),'_player_kept'])(i);   %Desicion in that round in hours
                P(i).partner(a)        =T.([pfg,num2str(r),'_player_vecino'])(i); %Partner ID in the session
                P(i).demand_partner(a) =T.([pfg,num2str(r),'_player_dem_vecino'])(i); %Partner demand in that round in hours
                P(i).kept_partner(a)   =T.([pfg,num2str(r),'_player_kept_vecino'])(i); %Partner desicion in that round in hours
                P(i).inspection(a)     =T.([pfg,num2str(r),'_player_fiscalizacion'])(i); % 0: Not was inspected- 1: was inspected in that round
                P(i).fine(a)           =T.([pfg,num2str(r),'_player_multa'])(i); % Fine paid in that round if inspected
                P(i).finalpayment(a)   =T.([pfg,num2str(r),'_player_finalpayment'])(i); % Money left each round
               
                % CALCULATED VARIABLES FROM RAW VARIABLES

                % DEPENDENT VARIABLES: Non-compliance  
                % Binary        
                P(i).NCbinary(a)=(P(i).kept(a)-12)>0; %0: Comply - 1:Not Comply
                % Hours
                P(i).NChours(a) =P(i).kept(a)-12;
                % Percentual
                if P(i).demand(a)>12
                    P(i).NCpercentual(a)=(P(i).kept(a)-12)./(P(i).demand(a)-12);            
                else
                    P(i).NCpercentual(a)=P(i).kept(a)-12;
                end                 

                % INDEPENDENT VARIABLES 

                % Exogenous Scarcity (experienced before making the decision): 
                P(i).Exo(a)=P(i).demand(a)-12;

                % Endogenous Scarcity: We have defined 2 types of endogenous scarcity
                % When participant had the second turn:
                % Endo = Endo2 = Hours above the quota used for the partner with the firs turn
                % When participant had the first turn: 
                % Endo = 0
                % Endo2= Endo from the last round that the participant had the second turn                
                if P(i).turn(a)==2                    
                    P(i).Endo(a) = max(0,P(i).kept_partner(a)-12);
                    P(i).Endo2(a) = P(i).Endo(a);
                    P(i).EndoB(a) = P(i).Endo(a)>0;
                else                                        
                    P(i).Endo(a) = 0; 
                    if a==1                
                        P(i).Endo2(a) = 0; 
                        P(i).EndoB(a) = 0;
                    else                
                        P(i).Endo2(a) = P(i).Endo2(a-1); 
                        P(i).EndoB(a) = P(i).EndoB(a-1);
                    end
                end

                % Endougenous Scarcity from past round:
                if r>(rp+1) % Exclude first round       
                    P(i).Endo_past(a)=P(i).Endo(a-1);
                    P(i).Endo_past2(a)=P(i).Endo2(a-1);
                    P(i).Endo_pastB(a)=P(i).EndoB(a-1);
                else                    
                    P(i).Endo_past(a)= 0;
                    P(i).Endo_past2(a)= 0;
                    P(i).Endo_pastB(a)= 0;
                end

                % Exogenous Scarcity Accumulated in past rounds:
                if r>(rp+2)
                    P(i).Exo_accu(a)=sum(P(i).Exo(1:(a-2))); % not including last round
                    P(i).Exo_accu_tot(a)=sum(P(i).Exo(1:(a-1))); % including last round   
                elseif r==(rp+2)
                    P(i).Exo_accu(a)=sum(P(i).Exo(1:(a-1)));  
                    P(i).Exo_accu_tot(a)=sum(P(i).Exo(1:(a-1))); 
                else
                    P(i).Exo_accu(a)=0;
                    P(i).Exo_accu_tot(a)=0;
                end

                % Endogenous Scarcity Accumulated in past rounds:
                if r>(rp+2)
                    P(i).Endo_accu(a)=sum(P(i).Endo(1:(a-2))); % not including last round
                    P(i).Endo_accuB(a)=sum(P(i).EndoB(1:(a-2))); % not including last round
                    P(i).Endo_accu_tot(a)=sum(P(i).Endo(1:a)); % including last round 
                    P(i).Endo_accu_totB(a)=sum(P(i).EndoB(1:a)); % including last round 
                elseif r==(rp+2)
                    P(i).Endo_accu(a)=sum(P(i).Endo(1:(a-1))); % not including last round 
                    P(i).Endo_accuB(a)=sum(P(i).EndoB(1:(a-1))); % not including last round                     
                    P(i).Endo_accu_tot(a)=sum(P(i).Endo(1:(a-1))); % including last round    
                    P(i).Endo_accu_totB(a)=sum(P(i).EndoB(1:(a-1))); % including last round    
                else
                    P(i).Endo_accu(a)=0;
                    P(i).Endo_accuB(a)=0;
                    P(i).Endo_accu_tot(a)=0;
                    P(i).Endo_accu_totB(a)=0;
                end

                % Endowment lost accumulated:
                if r>(rp+1)
                    P(i).Losses_accu(a)=10000-T.([pfg,num2str(r-1),'_player_finalpayment'])(i);
                    
                else
                    P(i).Losses_accu(a)=0;
                end

                %Inspection and Fine in the past round:
                if a>(rp+1)
                    P(i).Inspection_past(a) = P(i).inspection(a-1);
                    P(i).Fine_past(a) = P(i).fine(a-1)>0;
                end                
            end
        end
        BD=[BD,P]; % Join the data of this file with the rest of the database
    end
    
    % Remove data of one participant that left the experiment before finish in session 5
    if remove_data
        for i=1:length(BD)
            k12(i)=sum(BD(i).kept==12)==15; % ALL ROUNDS KEPT 12 (default according to the protocol)
        end
        BD(k12==1)=[];    
    end
    
    save(outfile_structure_m,'BD')
end
    
% CREATE CSV FILE    
if create_csv
    load(outfile_structure_m)
    vars=fieldnames(BD);
    R=[];
    S=[];
    for i=1:length(vars)    
        if length([BD.(vars{i})])==length(BD)
            Si = array2table([BD.(vars{i})]', 'VariableNames',vars(i));
            S=[S,Si];
            if ~strcmp(vars{i},'open_question') && ~strcmp(vars{i},'file')            
                for j=1:length(BD)
                    BD(j).(vars{i})=repmat((BD(j).(vars{i})),1,15);
                end
                Ri= array2table([BD.(vars{i})]', 'VariableNames',vars(i));
                R=[R,Ri];
            end
        else
            Ri= array2table([BD.(vars{i})]', 'VariableNames',vars(i));
            R=[R,Ri];
        end
    end

    % Write the tableS to CSV files
    writetable(R, outfile1_csv)
    writetable(S, outfile2_csv)
    save(outfile_table_m_rounds,'R')
    save(outfile_table_m_questions,'S')
end