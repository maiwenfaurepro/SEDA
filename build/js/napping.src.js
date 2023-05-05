
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"actvars","title":"(X,Y) Coordinates","type":"Variables","suggested":["continuous"],"permitted":["numeric"]},{"name":"quantisup","title":"Quantitative Supplementary Information","type":"Variables","suggested":["continuous"],"permitted":["numeric"]},{"name":"qualisup","title":"Categorical Supplementary Information","type":"Variables","suggested":["nominal","ordinal"],"permitted":["factor"]},{"name":"individus","title":"Stimuli Labels","type":"Variable","suggested":["nominal"],"permitted":["factor"]},{"name":"nFactors","title":"Number of Factors","type":"Integer","default":3},{"name":"corvar","title":"Correlations","type":"Bool","default":false},{"name":"contribvar","title":"Contributions","type":"Bool","default":false},{"name":"cosvar","title":"Cosine","type":"Bool","default":false},{"name":"coordind","title":"Coordinates","type":"Bool","default":false},{"name":"contribind","title":"Contributions","type":"Bool","default":false},{"name":"cosind","title":"Cosine","type":"Bool","default":false},{"name":"proba","title":"Significance threshold (%)","type":"Number","default":5},{"name":"abs","title":"X-axis","type":"Integer","default":1},{"name":"ord","title":"Y-axis","type":"Integer","default":2},{"name":"varact","title":"Active variables","type":"Bool","default":false},{"name":"varillus","title":"Supplementary variables","type":"Bool","default":false},{"name":"varactillus","title":"Active and supplementary variables","type":"Bool","default":true},{"name":"limcosvar","title":"Quality of representation (%)","type":"Number","default":0},{"name":"modact","title":"Individuals","type":"Bool","default":false},{"name":"modillus","title":"Categorical supplementary variables","type":"Bool","default":false},{"name":"modactillus","title":"Individuals and categorical supplementary variables","type":"Bool","default":true}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Analysis of Napping Data",
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
					label: "Stimuli Labels",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "individus",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "(X,Y) Coordinates",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "actvars",
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Quantitative Supplementary Information",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "quantisup",
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Categorical Supplementary Information",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "qualisup",
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
					type: DefaultControls.CollapseBox,
					typeName: 'CollapseBox',
					label: "Graphic Options",
					collapsed: true,
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
							label: "Individual Graphic Options",
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "modactillus"
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "modact"
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "modillus"
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Variable Graphic Options",
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "varactillus"
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "varact"
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "varillus"
								},
								{
									type: DefaultControls.TextBox,
									typeName: 'TextBox',
									name: "limcosvar",
									format: FormatDef.number
								}
							]
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
					label: "Numerical Indicators",
					collapsed: true,
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
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Individual Table Options",
							controls: [
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									margin: "large",
									stretchFactor: 1,
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "coordind"
										},
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "contribind"
										},
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "cosind"
										}
									]
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Variable Table Options",
							controls: [
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									margin: "large",
									stretchFactor: 1,
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "corvar"
										},
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "contribvar"
										},
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "cosvar"
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
