exitprompt = function() {
  return 'Are you sure you want to leave?';
};

// generate a random subject ID with 15 characters
var subject_id = jsPsych.randomization.randomID(15);
//var subject_id = "test";

// pick a random condition for the subject at the start of the experiment
var alignment_cond = jsPsych.randomization.sampleWithoutReplacement(['lowA', 'highA'], 1)[0];
var awareness_cond = jsPsych.randomization.sampleWithoutReplacement(['aware', 'naive'], 1)[0];
// var alignment_cond = 'lowA';
// var awareness_cond = 'aware';

urlvar = jsPsych.data.urlVariables();

if (typeof urlvar.alignment !== 'undefined') {
    alignment_cond = urlvar.alignment;
}

if (typeof urlvar.awareness !== 'undefined') {
    awareness_cond = urlvar.awareness;
}

// record the condition assignment in the jsPsych data
jsPsych.data.addProperties({
  subject: subject_id,
  alignment_cond: alignment_cond,
  awareness_cond: awareness_cond
});

/* create timeline */
var timeline = [];

/* init connection with pavlovia.org*/
var pavlovia_init = {
	type: "pavlovia",
	command: "init"
};
//timeline.push(pavlovia_init);

var test = {
	type: "html-button-response",
	stimulus: "hello",
	choices: [obj.button_response],
};
//timeline.push(test);

// PIS
var pis_sona = {
	type: "instructions",
	pages: [
        '<p>Welcome to the study.</p><p>Make sure to scroll down to the bottom of each page to see the navigation buttons.</p>',
        '<img src="img/pis1_sona.png"></img>',
        '<img src="img/pis2_sona.png"></img>',
        '<img src="img/pis3_sona.png"></img>'
    ],
  show_clickable_nav: true
};
//timeline.push(pis_sona);

var pis_online = {
	type: "instructions",
	pages: [
        '<p>Welcome to the study.</p><p>Make sure to scroll down to the bottom of each page to see the navigation buttons.</p>',
        '<img src="img/pis_online1.png" class="pis"></img>',
        '<img src="img/pis_online2.png" class="pis"></img>',
        '<img src="img/pis_online3.png" class="pis"></img>'
    ],
  show_clickable_nav: true
};
//timeline.push(pis_online);

var check_consent = function(elem) {
  if (document.getElementById('consent_checkbox').checked) {
    return true;
  }
  else {
    alert("If you wish to participate, you must check the box next to the statement 'I agree to participate in this study.'");
    return false;
  }
  return false;
};

// declare the block.
var consent = {
  type:'external-html',
  url: "js/consent.html",
  cont_btn: "start",
  check_fn: check_consent
};
//timeline.push(consent);

var contact = {
  type: 'survey-html-form',
  html: obj.contact
};
//timeline.push(contact);

var id = {
  type: 'survey-html-form',
  html: obj.recruitment.prolific
};
//timeline.push(id);

var demographics = {
	type: "survey-html-form",
	html: obj.demographics
};
//timeline.push(demographics);

/* define welcome message trial */
var welcome = {
	type: "html-button-response",
	stimulus: obj.welcome,
	choices: [obj.button_response]
};
timeline.push(welcome);

/* define instructions trial */
var instructions = {
	type: "html-button-response",
	stimulus: obj.instructions,
	choices: [obj.button_response]
};
timeline.push(instructions);

/* test trials */
var awareness = obj.awareness[awareness_cond];
var awareness_trial = {
	type: "html-button-response",
	stimulus: awareness,
	choices: [obj.button_response]
};
timeline.push(awareness_trial);

var projects = obj.projects[alignment_cond];
var prompt_array = [];
for (var i in projects) {
  question = {
    prompt: projects[i] + obj.project_question,
    name: "presentation_separate_cond_" + obj.proj_input[alignment_cond][i]
  };
  prompt_array.push(question);
}
var test_stimuli = prompt_array;

var separate = {
  type: 'survey-multi-choice',
  questions: [
	  {prompt: jsPsych.timelineVariable('prompt'),
	  options: obj.options,
	  required: obj.required,
	  horizontal: true,
    name: jsPsych.timelineVariable('name')
	  }
  ]
};

var separateBlock = {
	timeline: [separate],
	timeline_variables: test_stimuli,
	repetitions: 1,
	randomize_order: true
};
timeline.push(separateBlock);

var interstitial = {
	type: "html-button-response",
	stimulus: "Next you will see some more projects.",
	choices: ["Continue"]
};
timeline.push(interstitial);

var question_array = [];
for (var i in projects) {
  question = {
    prompt: projects[i],
    options: obj.options,
    required: obj.required,
    horizontal: true,
    name: "presentation_joint_cond_" + obj.proj_input[alignment_cond][i]
    };
  question_array.push(question);
}

var joint = {
    type: 'survey-multi-choice',
    questions: question_array,
    preamble: obj.preamble,
    randomize_question_order: true
  };
timeline.push(joint);

var aggregated_rep = {
  type: 'survey-multi-choice',
  questions: [
    {prompt: obj.aggregated_rep_prompt += "<p><img src='img/aggregated_rep.png'></p>",
    options: obj.options,
    required: obj.required,
    horizontal: true,
    name: obj.proj_input_rep
    }
  ]
};
timeline.push(aggregated_rep);

// check trials
var prompt_array = [];
for (var i in obj.check_trials) {
  question = {
    prompt: obj.check_trials[i],
    name: obj.check_input[i]
  };
  prompt_array.push(question);
}
var test_stimuli = prompt_array;

var check_trial = {
  type: 'survey-multi-choice',
  questions: [
	  {prompt: jsPsych.timelineVariable('prompt'),
	  options: obj.options,
	  required: obj.required,
	  horizontal: true,
    name: jsPsych.timelineVariable('name')
	  }
  ]
};

var check_block = {
	timeline: [check_trial],
	timeline_variables: test_stimuli,
	repetitions: 1,
	randomize_order: false
};
timeline.push(check_block);

// debrief
var debrief = {
	type: "instructions",
	pages: [
        '<img src="img/debrief1.png"></img>',
        '<img src="img/debrief2.png"></img>'
    ],
  show_clickable_nav: true
};
timeline.push(debrief);

var debrief_block = {
    type: 'html-button-response',
    stimulus: obj.end_message,
    choices: ['End experiment']
};
timeline.push(debrief_block);

/* finish connection with pavlovia.org*/
var pavlovia_finish = {
	type: "pavlovia",
	command: "finish",
	participantId: subject_id
};
// timeline.push(pavlovia_finish);

/* start the experiment */
jsPsych.init({
	timeline: timeline,
	experiment_width: 750,
	show_progress_bar: false,
	preload_images: obj.img_preload,
	on_finish: function() {
        onbeforeunload = "";
        alert("It will take a few seconds to save your data after you press 'OK'. You will then be automatically redirected to Prolific.");
        setTimeout("location.replace('https://app.prolific.co/submissions/complete?cc=8256C4AC');", 5000);
      }
});
