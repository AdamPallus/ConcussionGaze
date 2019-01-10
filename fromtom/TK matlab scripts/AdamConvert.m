
st=load('C:\Users\User\Documents\knight\ConcussionGaze\fromtom\CP48ST1 files\ST2017081705.mat');
esc=load('C:\Users\User\Documents\knight\ConcussionGaze\fromtom\CP48ST1 files\20170817-05.mat');

hhv = esc.Data(:,71);
rep = esc.Data(:,37);
lep = esc.Data(:,8);
time= esc.Data(:,1);

%experimental data
t=table(time,hhv,rep,lep,'variablenames',{'time','hhv','rep','lep'});


%add column for sample number
sample_rate = t.time(2);
t.sampletime = ceil(t.time/sample_rate);

%convert esc date format to number of seconds since midnight
datatime=split(esc.Date,':');
get_hour = split(datatime(1)," ");
datatime=str2num(datatime{2})*60+str2num(datatime{3})+360*str2num(get_hour{2}); %just take seconds
stimtime=360*st.systemtime(4)+st.systemtime(5)*60+st.systemtime(6);

file_diff = datatime- stimtime;

sttable = table(st.xposition, ...
                st.approxtime(1:end-1), ...
                'variablenames', ...
            {'raw_targ', 'approxtime'});
        
sttable.adjustedtime = sttable.approxtime - file_diff;
sttable.sampletime = ceil(sttable.adjustedtime/sample_rate);


tt= outerjoin(t,sttable,'key','sampletime','MergeKeys',true);
writetable(tt, 'combinedtable.csv')



%filter attempt



%filtering?
signal = t.hhv;
signal = signal - mean(signal(500:end));
f = 1;
[b,a]=butter(2,f/(1/sample_rate),'low'); 
[d,c]=butter(2,f/(1/sample_rate),'high');
V=filter(b,a,signal);
H=cumtrapz(t.time,V);
plot(H)

plot(cumtrapz(t.hhv))


%old
stimtable=table((st.xposition-640)*0.1,st.approxtime(1:end-1)-(datatime-stimtime),'variablenames',{'Targ','stime'});
stimtable.testtime=floor(stimtable.stime/0.0033)+1;

tt= outerjoin(t,stimtable,'key','time','MergeKeys',true);
tt(1:10,:)


%datelineup
experimentdate = 1:6;
for i = 1:length(escdate)
    experimentdate(i) = str2double(escdate{i});
end
experiment_time = datetime(experimentdate);
system_time = datetime(st.systemtime);
