% Experiment 2, distance discrimination task (180 trials).
% MATLAB R2023a + Psychtoolbox-3. Set the parameters in the block below before each session.
% Output: one text file per participant (columns described in the repository README).

clear;
close all;

%********************%
%%% Set before experiment %%%
pwl_spl_char = "_PWL";          % calibration identifier
date_char = "_YYYYMMDD";
id_char = "_XX";
name_char = "_INITIALS";
keyCloser  = 'RightArrow';      % "the second (probe) sound was closer"
keyFarther = 'LeftArrow';       % "the second (probe) sound was farther"
%********************%

%% Check keyboard IDs
keyboards = GetKeyboardIndices();
experimenterKeyboard = keyboards(1);
participantKeyboard = keyboards(2);

%% Result file
pathResult = [pwd, '/results/'];
initialTermExperiment = 'localizeTestPairwise_asDescribed';
resultFilename = append(pathResult, initialTermExperiment, pwl_spl_char, date_char, id_char, name_char);

%% Generate stimulus
fs = 48e3;
asDuration = 0.2;
nSamples = round(asDuration*fs);
t = (0:nSamples-1)/fs;

whitenoise = randn(1, nSamples);
whitenoise = 0.5 * whitenoise / max(abs(whitenoise));
am25 = whitenoise .* (1 + sin(2*pi*25*t));

% 5-ms sigmoidal onset/offset ramps
x = linspace(1, 0.005*fs, 0.005*fs);
yOnset = 1 ./ (1+exp((-1/20)*(x-100)));
yOffset = 1 - yOnset;
window = ones(1, nSamples);
window(1:0.005*fs) = yOnset;
window((length(window) - 0.005*fs + 1):length(window)) = yOffset;
s25 = am25 .* window;

isi = 0.5;                          

%% Initialize audio device
InitializePsychSound(1);
phd = PsychPortAudio('Open', 4, 1, 2, fs, 3, [], [], [], []);  

%% Trial setup
conditionNumber = 6;                % six ordered pairs
repetition = 30;                    % 30 repetitions per pair -> 180 trials
trialTotal = conditionNumber * repetition;
conditionTable = randperm(trialTotal);

% condition -> [reference location, probe location, answer (1 = probe closer, 0 = farther)]
% 1: near->mid, 2: near->far, 3: mid->near, 4: mid->far, 5: far->near, 6: far->mid
condDef = [1 2 0; 1 3 0; 2 1 1; 2 3 0; 3 1 1; 3 2 1];

[thisTrialTostore, thisConditionTostore, isCorrectTostore] = deal(zeros(trialTotal, 1));

%% Start
WaitSecs(0.2);
'Press any button to start experiment.'
KbWait(experimenterKeyboard);
'Start!'
WaitSecs(5);

%% Run trials
for i = 1:trialTotal
    cond = mod(conditionTable(i), 6);
    if cond == 0, cond = 6; end
    refLoc = condDef(cond, 1); probeLoc = condDef(cond, 2); answer = condDef(cond, 3);

    % One buffer: reference | 500 ms silence | probe; channels (near, mid, far)
    buf = zeros(3, 2*nSamples + nIsi);
    buf(refLoc, 1:nSamples) = s25;
    buf(probeLoc, nSamples+nIsi+1 : 2*nSamples+nIsi) = s25;

    PsychPortAudio('FillBuffer', phd, buf);
    onsetTime = PsychPortAudio('Start', phd, 1, 0, 1);
    probeOnset = onsetTime + asDuration + isi;

    % Response: accepted from the onset of the probe, no time limit, no feedback
    WaitSecs('UntilTime', probeOnset);
    [t_response, keyCode] = KbWait(participantKeyboard, []);

    cc = KbName(keyCode);
    if ~isempty(cc)
        if ~ischar(cc), cc = cc{1}; end
        if answer == 1
            isCorrect = double(strcmp(cc, keyCloser));
        else
            isCorrect = double(strcmp(cc, keyFarther));
        end
    else
        isCorrect = 0;
    end

    i
    isCorrect

    % The next trial begins 1 s after the response
    WaitSecs(1);

    thisTrialTostore(i) = i;
    thisConditionTostore(i) = cond;
    isCorrectTostore(i) = isCorrect;
end

%% Save
resultTable = table(thisTrialTostore, thisConditionTostore, isCorrectTostore);
writetable(resultTable, append(resultFilename, '.txt'));

'Finished!'
PsychPortAudio('Close');
