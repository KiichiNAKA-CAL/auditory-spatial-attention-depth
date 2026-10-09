% Experiment 2, spatial cueing task (630 trials).
% MATLAB R2023a + Psychtoolbox-3. Set the parameters in the block below before each session.
% Output: one text file per participant (columns described in the repository README).

clear;
close all;
PsychStartup;

%********************%
%%% Set before experiment %%%
pwl_spl_char = "_SPL";          % SPL calibration identifier (loudness-matched condition only)
id = 99;                        % Participant ID (odd: 25 Hz -> left key, even: 25 Hz -> right key)
idString = append("_", string(id));
keyLeft  = 'left';              % KbName of the left key
keyRight = 'right';             % KbName of the right key
%********************%

%% Audio device setting (adjust to your system)
audioDevID = audiodevinfo(0, 'Speakers (RME UFX III USB 3.0) (Windows DirectSound)');

%% Experiment info
dt = datetime('now', 'Format', 'yyyyMMddHHmmss');
dateString = string(dt);
pathResult = [pwd, '\results\']
initialTermExperiment = '_exogenous_asDescribed';
resultFilename = append(pathResult, dateString, initialTermExperiment, idString, pwl_spl_char, '.txt');

% Check keyboard IDs
keyboards = GetKeyboardIndices();
experimenterKeyboard = keyboards(1);
participantKeyboard = keyboards(1);

%% Generate stimuli
fs = 48e3;                          % 48 kHz
cueDuration = 0.02;                 % 20 ms cue
asDuration = 0.2;                   % 200 ms target
nCue = round(cueDuration*fs);
nTarget = round(asDuration*fs);
t = (0:nTarget-1)/fs;

% Target
whitenoise = randn(1, nTarget);
whitenoise = 0.5 * whitenoise / max(abs(whitenoise));
modDepth = 1;
am25 = whitenoise .* (1 + modDepth*sin(2*pi*25*t));
am75 = whitenoise .* (1 + modDepth*sin(2*pi*75*t));

% 5-ms sigmoidal onset/offset ramps
x = linspace(1, 0.005*fs, 0.005*fs);
yOnset = 1 ./ (1+exp((-1/20)*(x-100)));
yOffset = 1 - yOnset;
window = ones(1, nTarget);
windowCue = ones(1, nCue);
window(1:0.005*fs) = yOnset;
windowCue(1:0.005*fs) = yOnset;
window((length(window) - 0.005*fs + 1):length(window)) = yOffset;
windowCue((length(windowCue) - 0.005*fs + 1):length(windowCue)) = yOffset;

s25 = am25 .* window;
s75 = am75 .* window;

% Cue
cue = randn(1, nCue);
cue = 0.5 * cue / max(abs(cue));
cue = cue .* windowCue;

% 1000 Hz tone (1 s) as break signal, from the 1.3-m loudspeaker
t_tone = 0:1/fs:1;
y_tone = sin(2*pi*1000*t_tone);
tone = zeros(3, length(y_tone));
tone(2,:) = y_tone;

%% Experimental conditions
% 3 cue locations x 3 target locations x 2 SOAs = 18 cued conditions + 3 no-cue
% conditions = 21 conditions x 30 trials = 630 trials, random order
N_trial = (3*3*2+3)*30;
rndTrial = randperm(N_trial);
soaValues = [0.1 0.4];              % SOA code 1 -> 100 ms, 2 -> 400 ms

%% Initialize audio device
InitializePsychSound(1);
phd = PsychPortAudio('Open', audioDevID, 1, 4, fs, 3, [], [], [], []);

%% Start
WaitSecs(0.2);
'Press any button to start experiment.'
KbWait(experimenterKeyboard);
'Start!'
WaitSecs(5);

[tab_id, tab_amFreq, tab_cue, tab_target, tab_soa, tab_soaSec, tab_iti, tab_rt, tab_isCorrect] = deal(zeros(N_trial, 1));
nextOnset = 0;      % 0 = start the first trial immediately

%% Main experimental loop
for i = 1:N_trial
    % Condition from the trial code:
    % mod(c, 21) = 0-17 cued [cue, target, SOA], 18-20 no-cue [target]
    c = mod(rndTrial(i), 21);
    if c <= 17
        loc_cue    = floor(c/6) + 1;            % 1 near, 2 mid, 3 far
        loc_target = floor(mod(c, 6)/2) + 1;    % 1 near, 2 mid, 3 far
        dur_soa    = mod(c, 2) + 1;             % 1 = 100 ms, 2 = 400 ms
        present_soa = soaValues(dur_soa);
    else
        loc_cue    = 4;                         % no cue
        loc_target = c - 17;
        dur_soa    = 3;                         % code for no-cue
        present_soa = 0;                        
    end

    % Target AM rate (random)
    rndAmFreq = randi(2);
    if rndAmFreq == 1
        stim_freq = 25; target = s25;
    else
        stim_freq = 75; target = s75;
    end

    % One buffer per trial: cue at 0 and target exactly SOA later (no-cue
    % trials: target at 0); channels (1, 2, 3) = (near, mid, far)
    nSoa = round(present_soa*fs);
    trialBuffer = zeros(3, nSoa + nTarget);
    if loc_cue <= 3
        trialBuffer(loc_cue, 1:nCue) = cue;
    end
    trialBuffer(loc_target, nSoa+1:nSoa+nTarget) = target;

    % ITI: target offset -> next cue onset, uniform 1.8-2.8 s
    iti = 1.8 + rand;

    % Present. 'Start' with waitForStart = 1 blocks until the buffer starts and
    % returns the device-estimated onset time of the trial (cue, or target in
    % no-cue trials).
    PsychPortAudio('FillBuffer', phd, trialBuffer);
    cueOnset = PsychPortAudio('Start', phd, 1, nextOnset, 1);
    targetOnset = cueOnset + present_soa;
    nextOnset = targetOnset + asDuration + iti;

    % Responses accepted for up to 2 s after target onset
    % (key presses before target onset are ignored)
    WaitSecs('UntilTime', targetOnset);
    [t_response, keyCode] = KbWait(participantKeyboard, [], targetOnset + 2);
    responseTime = t_response - targetOnset;

    % Scoring: odd ID: 25 Hz -> left, 75 Hz -> right; even ID: reversed
    if mod(id, 2) == 1
        correctKey = {keyLeft, keyRight};
    else
        correctKey = {keyRight, keyLeft};
    end
    cc = KbName(keyCode);
    if ~isempty(cc)
        if ~ischar(cc), cc = cc{1}; end
        isCorrect = double(strcmp(cc, correctKey{rndAmFreq}));
    else
        isCorrect = 99;         % no response
        responseTime = 99;
    end

    % Display trial info
    i
    responseTime
    isCorrect

    % Store results
    tab_id(i) = id;
    tab_amFreq(i) = stim_freq;
    tab_cue(i) = loc_cue;
    tab_target(i) = loc_target;
    tab_soa(i) = dur_soa;
    tab_soaSec(i) = present_soa;
    tab_iti(i) = iti;
    tab_rt(i) = responseTime;
    tab_isCorrect(i) = isCorrect;

    % Break every 100 trials
    if mod(i, 100) == 0
        'Break!'
        PsychPortAudio('FillBuffer', phd, tone);
        PsychPortAudio('Start', phd, 1, 0, 1);
        WaitSecs(1);
        KbWait(participantKeyboard);

        'Start!'
        PsychPortAudio('FillBuffer', phd, tone);
        PsychPortAudio('Start', phd, 1, 0, 1);
        nextOnset = GetSecs + 3;        % resume 3 s after the start signal
    end
end

%% Save result
resultTable = table(tab_id, tab_amFreq, tab_cue, tab_target, tab_soa, tab_soaSec, tab_iti, tab_rt, tab_isCorrect);
writetable(resultTable, resultFilename);

'Finished!'
PsychPortAudio('Close');
