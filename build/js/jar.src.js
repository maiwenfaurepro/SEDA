
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"prodeff","title":"Stimulus Effect","type":"Variable","suggested":["nominal"],"permitted":["factor"]},{"name":"paneff","title":"Subject Effect","type":"Variable","suggested":["nominal"],"permitted":["factor"]},{"name":"likvar","title":"Liking Variable","type":"Variable","suggested":["continuous"],"permitted":["numeric"]},{"name":"sensoatt","title":"Sensory Attributes","type":"Variables","suggested":["nominal"],"permitted":["factor"]},{"name":"jarmod","title":"Coding of the JAR level","type":"String","default":"jar"}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Analysis of JAR data",
    jus: "3.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			typeName: 'VariableSupplier',
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Stimulus Effect",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "prodeff",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Subject Effect",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "paneff",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Liking Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "likvar",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Sensory Attributes",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "sensoatt",
							isTarget: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "jarmod",
					format: FormatDef.string
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
