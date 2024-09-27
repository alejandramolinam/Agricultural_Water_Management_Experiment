clear all
folder='../DATOS/';
files=dir([folder,'*.csv']);
outfile1_m = 'Datos_por_persona_20240731.mat';
outfile2_m = 'Datos_tesis.mat';
outfile1_csv = 'Datos_tesis_rondas.csv';
outfile2_csv = 'Datos_tesis_surveys.csv';
read_files = 1;
create_csv = 1;
chequear=1;
files=files([1:8]); %qué sesiones incluir


if read_files
BD=[];
for fi=1:length(files)
    
P=[];    
T=readtable([folder,files(fi).name]);

%vname={'id_in_group','payoff','demand','kept','left','turno','deficit',...
%       'perdida','vecino','kept_vecino','finalpayment','fiscalizacion','multa'};

rp=2; %rondas de prueba   


pf='agricultores_postpilotaje_ctrl_';
if isnan(T.([pf,num2str(1),'_player_tratamiento'])(1))
    pf='agricultores_postpilotaje_tto_';
end


for i=1:size(T,1)
    
    % Datos Identificadores
    P(i).sesion=fi; %ID Sesión
    P(i).file={files(fi).name}; %Archivo de datos sesión
    P(i).tto=T.([pf,num2str(1),'_player_tratamiento'])(i); % Asignación Tto
    %P(i).player= T.('participant_id_in_session')(i);        % ID Player
    P(i).player = T.('participant_code')(i); % ID Player    
    % Datos Survey Inicial
    P(i).conoc_derechos=T.survey_inicial_1_player_conocimiento_derechos(i);
    P(i).derecho=T.survey_inicial_1_player_derecho(i);
    P(i).hectareas=T.survey_inicial_1_player_hectareas(i);
    P(i).edad=T.survey_inicial_1_player_edad(i);
    P(i).escolaridad=T.survey_inicial_1_player_escolaridad(i);
    if strcmp(T.survey_inicial_1_player_genero(i),'Masculino')  % HOMBRE=0 MUJER =1
        genero=0;
    else
        genero=1;
    end        
    P(i).genero=genero;
    P(i).risk_av=T.survey_inicial_1_player_risk_aversion(i);
    P(i).pre_beliefs_igual=T.survey_inicial_1_player_pre_beliefs_igual(i);
    P(i).pre_beliefs_menos=T.survey_inicial_1_player_pre_beliefs_menos(i);
    P(i).pre_beliefs_mas=T.survey_inicial_1_player_pre_beliefs_mas(i); 
    
    % Datos Survey Intermedio
    P(i).inter_beliefs_igual=T.([pf,'7_player_pre_beliefs_igual'])(i);
    P(i).inter_beliefs_menos=T.([pf,'7_player_pre_beliefs_menos'])(i);
    P(i).inter_beliefs_mas=T.([pf,'7_player_pre_beliefs_mas'])(i);
    
    % Datos Survey Final
    P(i).post_beliefs_igual=T.survey_final_1_player_post_beliefs_igual(i);
    P(i).post_beliefs_menos=T.survey_final_1_player_post_beliefs_menos(i);
    P(i).post_beliefs_mas=T.survey_final_1_player_post_beliefs_mas(i);    
    P(i).causa_autoridad=T.survey_final_1_player_causa_autoridad(i);
    P(i).causa_injusticia=T.survey_final_1_player_causa_injusticia(i);
    P(i).causa_multa=T.survey_final_1_player_causa_multa(i);
    P(i).causa_necesidad=T.survey_final_1_player_causa_necesidad(i);
    P(i).causa_otros=T.survey_final_1_player_causa_otros(i);
    P(i).pregunta_abierta=T.survey_final_1_player_pregunta_abierta(i);
    P(i).dif_beliefs_mas=P(i).post_beliefs_mas-P(i).pre_beliefs_mas;
    
    % Rondas Juego
    for r=(rp+1):17
        a=r-rp;
        
        % VARIABLES CRUDAS
        P(i).ronda(a)=a;
        P(i).turno(a)=T.([pf,num2str(r),'_player_turno'])(i); % Tipo de Turno 1 o 2        
        P(i).demanda(a)=T.([pf,num2str(r),'_player_demand'])(i); 
        P(i).kept(a)=T.([pf,num2str(r),'_player_kept'])(i);
        P(i).deficit(a)=T.([pf,num2str(r),'_player_deficit'])(i);
        P(i).vecino(a)=T.([pf,num2str(r),'_player_vecino'])(i);
        P(i).demanda_vecino(a)=T.([pf,num2str(r),'_player_dem_vecino'])(i);
        P(i).kept_vecino(a)=T.([pf,num2str(r),'_player_kept_vecino'])(i);
        P(i).fiscalizacion(a)=T.([pf,num2str(r),'_player_fiscalizacion'])(i);
        P(i).multa(a)=T.([pf,num2str(r),'_player_multa'])(i);
       
      
        % CALCULOS DE VARIABLES
        
        % VARIABLE DEPENDIENTE: Sobreconsumo 
        % BINARIA        
        P(i).ybinaria(a)=(P(i).kept(a)-12)>0;
        % HORAS
        P(i).yhoras(a)=P(i).kept(a)-12;
        % PORCENTUAL
        if P(i).demanda(a)>12
            P(i).yporcentual(a)=(P(i).kept(a)-12)./(P(i).demanda(a)-12);            
        else
            P(i).yporcentual(a)=P(i).kept(a)-12;
        end                 
        
        % VARIABLES INDEPENDIENTES
        % Calculos de escasez dependiendo turno:
        
        % Escasez exogena inicial del turno (antes de decidir):
        P(i).Exo(a)=P(i).demanda(a)-12;
        
        % Escasez post decision:
        if P(i).turno(a)==1
            P(i).Exo_efectiva(a) = P(i).demanda(a) - P(i).kept(a);
            P(i).End(a) = 0;
        else
            P(i).Exo_efectiva(a) = P(i).Exo(a); % Lo que hubiese faltado si el vecino cumple el turno
            P(i).End(a) = max(0,P(i).kept_vecino(a)-12);
        end
        
        % Escasez turno anterior:
        if r>(rp+1)
            P(i).Exo_ant(a)=P(i).Exo_efectiva(a-1);
            if P(i).turno(a-1)==2                
                P(i).End_ant(a)=P(i).End(a-1);
                P(i).Dt2_ant(a)=1;
            else
                P(i).End_ant(a)=0;
                P(i).Dt2_ant(a)=0;
            end
        else
            P(i).Exo_ant(a)= 0;
            P(i).End_ant(a)= 0;
            P(i).Dt2_ant(a)=0;
        end
        
        % Escasez exogena acumulada:
        if r>(rp+1)
            P(i).Exo_acu(a)=sum(P(i).Exo_ant(1:(a-1)));
            P(i).Exo_acu_tot(a)=sum(P(i).Exo_ant(1:a));
        else
            P(i).Exo_acu(a)=0;
            P(i).Exo_acu_tot(a)=0;
        end
        
        % Escasez endogena acumulada
        if r>(rp+1)
            P(i).End_acu(a)=sum(P(i).End_ant(1:(a-1)));
            P(i).End_acu_tot(a)=sum(P(i).End_ant(1:a));
        else
            P(i).End_acu(a)=0;
            P(i).End_acu_tot(a)=0;
        end
        
        % Perdida acumulada:
        if r>(rp+1)
            P(i).Perd_acu(a)=10000-T.([pf,num2str(r-1),'_player_finalpayment'])(i);
            %P(i).Perd_acu(a)=100*P(i).deficit(a)+;
        else
            P(i).Perd_acu(a)=0;
        end
        
        %Post multa:
        if a>(rp+1)
            P(i).multa_ronda_anterior(a) = P(i).multa(a-1)>0;
        end
        
        %Demandas de ambas personas
        if P(i).tto==1
            P(i).demanda_sumada(a)=P(i).demanda(a)+P(i).demanda_vecino(a);
            P(i).demanda_diferencia(a)=P(i).demanda(a)-P(i).demanda_vecino(a);
        else
            P(i).demanda_sumada(a)=NaN;
            P(i).demanda_diferencia(a)=NaN;
        end
                      
    end
end

BD=[BD,P];

end

% Sacar datos Cami en sesión 5 (segunda de los agricultores)
if chequear
    for i=1:length(BD)
        k12(i)=sum(BD(i).kept==12)==15;
    end
    BD(i)=[];    
end

save(outfile1_m,'BD')

end

    
    
if create_csv
load(outfile1_m)
vars=fieldnames(BD);
R=[];
S=[];
for i=1:length(vars)    
    if length([BD.(vars{i})])==length(BD)
        Si = array2table([BD.(vars{i})]', 'VariableNames',vars(i));
        S=[S,Si];
        if ~strcmp(vars{i},'pregunta_abierta') && ~strcmp(vars{i},'file')            
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

save(outfile2_m, 'R' ,'S')
 
% Write the table to a CSV file
writetable(R, outfile1_csv)
writetable(S, outfile2_csv)
end