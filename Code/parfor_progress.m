% PARFOR_PROGRESS
%   Example of generating a progress table to display during iterations of
%   a `parfor` loop
%
%   FUNCTIONS:
%       code_time
%       display_progress

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Setup and Flags

% Start code timer to measure full run time of driver file, used as an
% argument to the code_time() helper function
tic_driver = tic;

% Flags
n_message = 20;         % Number of progress messages to display

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Simulation

% Start timer for simulation code
tic_sim = tic;

% Setup variables for parfor loop
n = 1000;
A = 500;
a = zeros(1, n);

% Initialize the progress message function, create a data queue
% object to receive messages from parallel workers, and display
% progress messages throughout code execution
display_progress(n, n_message, tic_sim)
queue = parallel.pool.DataQueue;
afterEach(queue, @display_progress)

% Example parfor loop from MATLAB documentation
parfor i = 1:n
    % Lengthy calculation for example
    a(i) = max(abs(eig(rand(A))));

    % Ping the data queue after each simulation is complete to
    % update the progress table
    send(queue, [])
end

% Display total time elapsed for driver file
elapsed = code_time(tic_driver);
fprintf("Driver completed in %s.\n\n", elapsed)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper Functions

% Determine time elapsed since the start timer and the current system time
function [elapsed, current] = code_time(start_tic)

% toc gives the time elapsed since tic was called; if toc is given a
% specific start tic, it calculates the time elapsed since that tic
elapsed = toc(start_tic);

% Format the elapsed time in HH:mm:ss to match the current time format
hh = floor(elapsed / 3600);
mm = floor((elapsed - hh * 3600)/60);
ss = round(elapsed - hh * 3600 - mm * 60);

elapsed = compose("%02d:%02d:%02d", hh, mm, ss);

% Determine the current system time and format it in HH:mm:ss
current = string(datetime("now", "Format", "HH:mm:ss"));

end

% Create a formatted progress table to update as the code progresses
function display_progress(n_total, n_message, start_tic)

% Define persistent variables that will be stored in memory outside of the
% function - the previous value will be available the next time the
% function is called
persistent N N_digit N_done per_message tic0

% The function is only called with three arguments when the table is being
% initialized
if nargin == 3

    % Define the base values for the persistent variables
    N = n_total;
    N_digit = ceil(log10(abs(N) * (1 + eps(N))));
    N_done = 0;
    per_message = round(N/n_message);
    tic0 = start_tic;

    % Determine the current system time and display the start message
    [~, current] = code_time(tic0);
    fprintf( ...
        "\n==========  Total Iterations: %d     " + ...
        "Start Time: %s  ==========\n\n", ...
        N, current);

else
    % Increment the counter of the number of times this function has been
    % called
    % Since the function is called every time an iteration is completed,
    % this is also the count of the number of done iterations
    N_done = N_done + 1;

    % Display a new row of the progress table every time a certain number
    % of iterations is compleeted (based on the number of messages
    % requested)
    if mod(N_done, per_message) == 0

        % Determine the amount of time elapsed since the start tic and the
        % current system time for the progress table
        [elapsed, current] = code_time(tic0);

        % The first message displayed needs to include the table header
        if N_done == per_message
            
            % Table header row and dividing line
            fprintf( ...
                "     Completed%*s|" + ...
                "   Elapsed    |" + ...
                "   Time\n", ...
                N_digit+2, "");
            fprintf("   %s\n", strjoin(repmat("-", N_digit+42, 1), ""))

            % First data row in the progress table
            fprintf(...
                "     %*d ( %2.0f%% )   |" + ...
                "   %s   |" + ...
                "   %s\n", ...
                N_digit, N_done, N_done/N * 100, ...
                elapsed, current);

        % The final message displayed has one fewer space to accommodate
        % the extra digit in 100%
        elseif N_done == N

            % Final data row in the progress table
            fprintf(...
                "     %*d ( %2.0f%% )  |" + ...
                "   %s   |" + ...
                "   %s\n", ...
                N_digit, N_done, N_done/N * 100, ...
                elapsed, current);

            % Dividing line under the progress table
            fprintf("   %s\n\n", strjoin(repmat("-", N_digit+42, 1), ""))
        
        % All other table rows
        else
            % Standard data row in the progress table
            fprintf(...
                "     %*d ( %2.0f%% )   |" + ...
                "   %s   |" + ...
                "   %s\n", ...
                N_digit, N_done, N_done/N * 100, ...
                elapsed, current);
        end
    end
end
end
