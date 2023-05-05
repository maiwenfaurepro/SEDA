
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"prod","title":"Stimulus Variable","type":"Variable","suggested":["nominal"],"permitted":["factor"]},{"name":"pane","title":"Subject Variable","type":"Variable","suggested":["nominal"],"permitted":["factor"]},{"name":"senso","title":"Sensory Attributes","type":"Variables","suggested":["continuous"],"permitted":["numeric"]},{"name":"thresh","title":"Threshold (%)","type":"Number","default":5,"min":0,"max":100},{"name":"nbsimul","title":"Number of panels","type":"Number","default":300,"min":10,"max":1000},{"name":"nbpane","title":"Number of subjects","type":"Number","default":20,"min":2},{"name":"ind_gr_box","title":"Individual variability around stimuli","type":"Bool","default":false},{"name":"var_gr_box","title":"Variability around sensory attributes","type":"Bool","default":false},{"name":"abs","title":"X-axis","type":"Integer","default":1},{"name":"ord","title":"Y-axis","type":"Integer","default":2},{"name":"scale_unit_box","title":"Scale to unit variance","type":"Bool","default":true},{"name":"center_pane_box","title":"Center by subject","type":"Bool","default":true},{"name":"scale_pane_box","title":"Scale by subject","type":"Bool","default":false},{"name":"level_search","title":"p-value of the Stimulus effect (%)","type":"Number","default":20,"min":1,"max":100},{"name":"nFactors","title":"Number of factors","type":"Integer","default":3},{"name":"proba","title":"Significance threshold (%)","type":"Number","default":5}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Multivariate Representation of the Stimulus Space",
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
					label: "Stimulus Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "prod",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Subject Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "pane",
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
							name: "senso",
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
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Standardization of the Sensory Attributes",
					stretchFactor: 1,
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "scale_unit_box"
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Selection of the Sensory Attributes",
					stretchFactor: 1,
					controls: [
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							name: "level_search",
							format: FormatDef.number
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.CollapseBox,
					typeName: 'CollapseBox',
					label: "Graphic Options",
					collapsed: true,
					stretchFactor: 1,
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Components to Plot",
							controls: [
								{
									type: DefaultControls.Label,
									typeName: 'Label',
									label: "",
									cell: {"column":0,"row":0},
									stretchFactor: 1,
									controls: [
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "abs",
											format: FormatDef.number
										}
									]
								},
								{
									type: DefaultControls.Label,
									typeName: 'Label',
									label: "",
									cell: {"column":1,"row":0},
									stretchFactor: 1,
									controls: [
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "ord",
											format: FormatDef.number
										}
									]
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Additional Plots",
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "ind_gr_box"
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "var_gr_box"
								}
							]
						}
					]
				},
				{
					type: DefaultControls.CollapseBox,
					typeName: 'CollapseBox',
					label: "Resampling Options",
					collapsed: true,
					stretchFactor: 1,
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "",
							cell: {"column":0,"row":0},
							stretchFactor: 1,
							controls: [
								{
									type: DefaultControls.TextBox,
									typeName: 'TextBox',
									name: "nbpane",
									format: FormatDef.number
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "",
							cell: {"column":0,"row":1},
							stretchFactor: 1,
							controls: [
								{
									type: DefaultControls.TextBox,
									typeName: 'TextBox',
									name: "nbsimul",
									format: FormatDef.number
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "",
							cell: {"column":0,"row":2},
							stretchFactor: 1,
							controls: [
								{
									type: DefaultControls.TextBox,
									typeName: 'TextBox',
									name: "thresh",
									format: FormatDef.number
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "",
							cell: {"column":1,"row":0},
							stretchFactor: 1,
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "center_pane_box"
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "",
							cell: {"column":1,"row":1},
							stretchFactor: 1,
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "scale_pane_box"
								}
							]
						}
					]
				},
				{
					type: DefaultControls.CollapseBox,
					typeName: 'CollapseBox',
					label: "Numerical Indicators",
					collapsed: true,
					stretchFactor: 1,
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Automatic Description of the Axes",
							controls: [
								{
									type: DefaultControls.Label,
									typeName: 'Label',
									label: "",
									cell: {"column":0,"row":0},
									stretchFactor: 1,
									controls: [
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "proba",
											format: FormatDef.number
										}
									]
								},
								{
									type: DefaultControls.Label,
									typeName: 'Label',
									label: "",
									cell: {"column":1,"row":0},
									stretchFactor: 1,
									controls: [
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "nFactors",
											format: FormatDef.number
										}
									]
								}
							]
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
