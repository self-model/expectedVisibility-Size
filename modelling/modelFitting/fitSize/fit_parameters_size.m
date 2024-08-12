clear all;
warning off;
rng(1);

df = readtable('data/E_size.csv');

T=140; % number of time points to simulate
dt=0.05; % the duration of each time point, in seconds
nseeds=12;
nparams = 10;
nsubjects = length(unique(df.subj_id));

df.present(df.present==-1) =0;
df.decision = double((df.correct == 1 & df.present == 1) | (df.correct == 0 & df.present == 0));
df.rt = round(df.rt / dt);
df = df(df.rt<=T-1,:);

subj_data = zeros(nsubjects,2,2,2,T);

for s = 1:nsubjects
    df_filtered = df(df.subj_id == s, :);
    numRows = height(df_filtered);
    for i = 1:numRows
        size_index = df_filtered.easy(i)+1;
        present_index = df_filtered.present(i)+1;
        decision_index = df_filtered.decision(i)+1;
        rt_index = df_filtered.rt(i)+1;
        subj_data(s,size_index,present_index,decision_index,rt_index)=...
        subj_data(s,size_index,present_index,decision_index,rt_index)+1;
    end
end

%parameter order: [theta0, theta1, believed theta0,
%believed theta1, gamma, minimal ndt (in seconds), ndt range (in seconds),
%alpha, believed alpha, softmax temperature
lb = [-1 -1 -1 -1 0.45,0.2,0.1,-0.4,-0.4, -0.65];
ub = [-0.1 -0.1 -0.1 -0.1 1,1,1,0.4,0.4,0];
initialParams = lb+rand(nseeds,10).*(ub-lb); 

A = []; b = []; Aeq = []; beq = [];
nonlcon = [];

if isfile("E_size_fit.mat")
    load("E_size_fit.mat");
    first_seed = find(isnan(mean(LL)),1,"first");
else
    fitted_parameters = nan([nsubjects,nparams,nseeds]);
    LL = nan([nsubjects,nseeds]);
    first_seed = 1;
end

for seed=first_seed:nseeds

    seed
    parfor s=1:nsubjects

    
        s
        data = squeeze(subj_data(s,:,:,:,:));
        objective = @(params) get_neg_likelihood_softmax(params, data,dt,T);
    
        % SIMULATED ANNEALING
        options = optimoptions('simulannealbnd','InitialTemperature',1000,...
            'TemperatureFcn','temperatureboltz', 'HybridFcn','fmincon');
        [optimizedParams, fval] = simulannealbnd(objective, initialParams(seed,:), lb, ub, options)
    
        fitted_parameters(s,:,seed)=optimizedParams;
        LL(s,seed)=fval;

    end
    save('E_size_fit','fitted_parameters', 'LL', 'subj_data');
end

[~, idx] = min(LL, [], 2);
best_parameters = nan(nsubjects,10);

for i=1:size(best_parameters,1)
    best_parameters(i,:) = fitted_parameters(i,:,idx(i));
end

save('E_size_fit','fitted_parameters', 'LL', 'subj_data', 'best_parameters');

exit