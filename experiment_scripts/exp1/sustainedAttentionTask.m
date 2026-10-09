% Experiment 1, sustained attention task (one block per run).
% MATLAB R2023a + Psychtoolbox-3. Set the parameters in the block below before each session.
% Output: one text file per block (columns described in the repository README).

clear;
close all;

%********************%
%%% Set before experiment %%%
TRIALNUMBER = '_XXX';           % Trial number identifier (e.g., '_001')
thisCondition = 1;              % Standard location: 1=near (0.3 m), 2=mid (1.3 m), 3=far (2.3 m)
pwl_spl_char = "_SPLX";         % SPL condition identifier (e.g., "_SPL1" unmatched, "_SPL2" matched)
date_char = "_YYYYMMDD";        % Date identifier
id_char = "_XX";                % Participant ID
name_char = "_INITIALS";        % Participant initials
counterbalance = 1;             % 1: right key = 25 Hz, left key = 75 Hz 2: reversed
keyRight = 'RightArrow';        % KbName of the right key of the 2-key keyboard
keyLeft  = 'LeftArrow';         % KbName of the left key
%********************%

'This program runs one block (one standard location x one SPL condition).'
'Did you confirm the id? Is counter balance correct?'

%% Check keyboard IDs
keyboards = GetKeyboardIndices();
experimenterKeyboard = keyboards(1);
participantKeyboard = keyboards(2);

%% Result file
id = 1;
pathResult = [pwd, '/results/'];
initialTermExperiment = 'occasionalAttentionShift_asDescribed';
resultFilename = append(pathResult, initialTermExperiment, pwl_spl_char, date_char, id_char, name_char, TRIALNUMBER);

%% Generate stimuli
fs = 48e3;                          % 48 kHz
asDuration = 0.2;                   % 200 ms
nSamples = round(asDuration*fs);
t = (0:nSamples-1)/fs;

% One zero-mean white noise sample, shared by both stimuli
whitenoise = randn(1, nSamples);
whitenoise = 0.5 * whitenoise / max(abs(whitenoise));

% Sinusoidal amplitude modulation at 25 or 75 Hz, modulation depth 100%
modDepth = 1;
am25 = whitenoise .* (1 + modDepth*sin(2*pi*25*t));
am75 = whitenoise .* (1 + modDepth*sin(2*pi*75*t));

% 5-ms sigmoidal onset/offset ramps
x = linspace(1, 0.005*fs, 0.005*fs);
yOnset = 1 ./ (1+exp((-1/20)*(x-100)));
yOffset = 1 - yOnset;
window = ones(1, nSamples);
window(1:0.005*fs) = yOnset;
window((length(window) - 0.005*fs + 1):length(window)) = yOffset;

s25 = am25 .* window;
s75 = am75 .* window;

%% Prepare multi-channel audio: channel (1, 2, 3) = (near, mid, far)
% stimuli{location, stimulusType}; stimulusType 1 = 25 Hz, 2 = 75 Hz
stimuli = cell(3, 2);
for loc = 1:3
    buf25 = zeros(3, nSamples); buf25(loc, :) = s25; stimuli{loc, 1} = buf25;
    buf75 = zeros(3, nSamples); buf75(loc, :) = s75; stimuli{loc, 2} = buf75;
end

%% Trial setup  [paper 2.1.3]
% 110 stimuli per block: trials -9..0 = 10 warm-up trials at the standard
% location; 1..100 = 90 standard (p = .90) + 5 + 5 shifted (p = .05/location)
trials = linspace(-9, 100, 110);
rndTrial = randperm(100);
shifted = setdiff(1:3, thisCondition);

%% Initialize audio device
InitializePsychSound(1);
phd = PsychPortAudio('Open', 4, 1, 2, fs, 3, [], [], [], []);

%% Start
WaitSecs(0.2);
'Press any button to start experiment.'
KbWait(experimenterKeyboard);
'Start!'
WaitSecs(5);

nTrials = length(trials);
[idTostore, thisConditionTostore, thisTrialTostore, whichLoudspeakerTostore, ...
    whichStimulusTostore, responseTimeTostore, isCorrectTostore, itiTostore] = deal(zeros(nTrials, 1));
nextOnset = 0;      % 0 = start the first stimulus immediately

for j = 1:nTrials
    thisTrial = trials(j);

    % Loudspeaker: warm-up -> standard; otherwise 90% standard, 5% each shifted
    % location (a residue of randperm(100) mod 20 equal to a location index
    % selects that location; each residue occurs exactly 5 times in 100 trials)
    if thisTrial < 1
        whichLoudspeaker = thisCondition;
    else
        lll = mod(rndTrial(thisTrial), 20);
        if lll == shifted(1)
            whichLoudspeaker = shifted(1);
        elseif lll == shifted(2)
            whichLoudspeaker = shifted(2);
        else
            whichLoudspeaker = thisCondition;
        end
    end

    % Stimulus type (random): 1 = 25 Hz, 2 = 75 Hz
    whichStimulus = randi(2);
    asToPlay = stimuli{whichLoudspeaker, whichStimulus};

    % ITI for this trial: offset of this stimulus -> onset of the next, uniform 1.8-2.8 s
    iti = 1.8 + rand;

    % Present the stimulus at the scheduled onset. 'Start' with waitForStart = 1
    % blocks until the sound starts and returns the device-estimated onset time.
    PsychPortAudio('FillBuffer', phd, asToPlay);
    onsetTime = PsychPortAudio('Start', phd, 1, nextOnset, 1);
    nextOnset = onsetTime + asDuration + iti;

    % Responses are accepted until the onset of the next stimulus
    [t_response, keyCode] = KbWait(participantKeyboard, [], nextOnset);
    responseTime = t_response - onsetTime;

    % Scoring with the counterbalanced key mapping
    if counterbalance == 1
        correctKey = {keyRight, keyLeft};   % 25 Hz -> right, 75 Hz -> left
    else
        correctKey = {keyLeft, keyRight};   % 25 Hz -> left, 75 Hz -> right
    end
    cc = KbName(keyCode);
    if ~isempty(cc)
        if ~ischar(cc), cc = cc{1}; end
        isCorrect = double(strcmp(cc, correctKey{whichStimulus}));
    else
        isCorrect = 0;          % no response: scored as an error
        responseTime = 100;     % timeout value
    end
    responseTime
    isCorrect

    % Store results
    idTostore(j) = id;
    thisConditionTostore(j) = thisCondition;
    thisTrialTostore(j) = thisTrial;
    whichLoudspeakerTostore(j) = whichLoudspeaker;
    whichStimulusTostore(j) = whichStimulus;
    responseTimeTostore(j) = responseTime;
    isCorrectTostore(j) = isCorrect;
    itiTostore(j) = iti;
end

%% Save
resultTable = table(idTostore, thisConditionTostore, thisTrialTostore, whichLoudspeakerTostore, ...
    whichStimulusTostore, responseTimeTostore, isCorrectTostore, itiTostore);
standardNames = {'_standardNear', '_standardMid', '_standardFar'};
writetable(resultTable, append(resultFilename, standardNames{thisCondition}, '.txt'));

'Finished!'
PsychPortAudio('Close');
