namespace WaterSimDCDC.Processes
{
    partial class AlterGPCDFeedbackProcessForm
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.buttonOK = new System.Windows.Forms.Button();
            this.textBox_ProcessName = new System.Windows.Forms.TextBox();
            this.label_ProcessName = new System.Windows.Forms.Label();
            this.textBoxMaxDeficit = new System.Windows.Forms.TextBox();
            this.labelMaxDeficit = new System.Windows.Forms.Label();
            this.textBoxMinGPCD = new System.Windows.Forms.TextBox();
            this.labelMinGPCD = new System.Windows.Forms.Label();
            this.toolStrip1 = new System.Windows.Forms.ToolStrip();
            this.StatusLabel = new System.Windows.Forms.ToolStripLabel();
            this.toolStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // buttonOK
            // 
            this.buttonOK.Location = new System.Drawing.Point(255, 234);
            this.buttonOK.Name = "buttonOK";
            this.buttonOK.Size = new System.Drawing.Size(75, 23);
            this.buttonOK.TabIndex = 0;
            this.buttonOK.Text = "OK";
            this.buttonOK.UseVisualStyleBackColor = true;
            this.buttonOK.Click += new System.EventHandler(this.buttonOK_Click);
            // 
            // textBox_ProcessName
            // 
            this.textBox_ProcessName.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.textBox_ProcessName.Location = new System.Drawing.Point(122, 12);
            this.textBox_ProcessName.Name = "textBox_ProcessName";
            this.textBox_ProcessName.Size = new System.Drawing.Size(208, 23);
            this.textBox_ProcessName.TabIndex = 1;
            this.textBox_ProcessName.TextChanged += new System.EventHandler(this.textBox_ProcessName_TextChanged);
            // 
            // label_ProcessName
            // 
            this.label_ProcessName.AutoSize = true;
            this.label_ProcessName.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label_ProcessName.Location = new System.Drawing.Point(16, 15);
            this.label_ProcessName.Name = "label_ProcessName";
            this.label_ProcessName.Size = new System.Drawing.Size(100, 17);
            this.label_ProcessName.TabIndex = 2;
            this.label_ProcessName.Text = "Process Name";
            // 
            // textBoxMaxDeficit
            // 
            this.textBoxMaxDeficit.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.textBoxMaxDeficit.Location = new System.Drawing.Point(140, 49);
            this.textBoxMaxDeficit.Name = "textBoxMaxDeficit";
            this.textBoxMaxDeficit.Size = new System.Drawing.Size(190, 23);
            this.textBoxMaxDeficit.TabIndex = 3;
            this.textBoxMaxDeficit.TextChanged += new System.EventHandler(this.textBoxMaxDeficit_TextChanged);
            // 
            // labelMaxDeficit
            // 
            this.labelMaxDeficit.AutoSize = true;
            this.labelMaxDeficit.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.labelMaxDeficit.Location = new System.Drawing.Point(21, 52);
            this.labelMaxDeficit.Name = "labelMaxDeficit";
            this.labelMaxDeficit.Size = new System.Drawing.Size(113, 17);
            this.labelMaxDeficit.TabIndex = 4;
            this.labelMaxDeficit.Text = "% Deficit Trigger";
            // 
            // textBoxMinGPCD
            // 
            this.textBoxMinGPCD.AccessibleDescription = "GPCD will not be lowered below this value";
            this.textBoxMinGPCD.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.textBoxMinGPCD.Location = new System.Drawing.Point(122, 88);
            this.textBoxMinGPCD.Name = "textBoxMinGPCD";
            this.textBoxMinGPCD.Size = new System.Drawing.Size(208, 23);
            this.textBoxMinGPCD.TabIndex = 5;
            this.textBoxMinGPCD.TextChanged += new System.EventHandler(this.textBoxMinGPCD_TextChanged);
            // 
            // labelMinGPCD
            // 
            this.labelMinGPCD.AutoSize = true;
            this.labelMinGPCD.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.labelMinGPCD.Location = new System.Drawing.Point(12, 91);
            this.labelMinGPCD.Name = "labelMinGPCD";
            this.labelMinGPCD.Size = new System.Drawing.Size(106, 17);
            this.labelMinGPCD.TabIndex = 6;
            this.labelMinGPCD.Text = "Minimum GPCD";
            // 
            // toolStrip1
            // 
            this.toolStrip1.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.toolStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.StatusLabel});
            this.toolStrip1.Location = new System.Drawing.Point(0, 261);
            this.toolStrip1.Name = "toolStrip1";
            this.toolStrip1.Size = new System.Drawing.Size(342, 25);
            this.toolStrip1.TabIndex = 7;
            this.toolStrip1.Text = "toolStrip1";
            // 
            // StatusLabel
            // 
            this.StatusLabel.Name = "StatusLabel";
            this.StatusLabel.Size = new System.Drawing.Size(0, 22);
            // 
            // AlterGPCDFeedbackProcessForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(342, 286);
            this.Controls.Add(this.toolStrip1);
            this.Controls.Add(this.labelMinGPCD);
            this.Controls.Add(this.textBoxMinGPCD);
            this.Controls.Add(this.labelMaxDeficit);
            this.Controls.Add(this.textBoxMaxDeficit);
            this.Controls.Add(this.label_ProcessName);
            this.Controls.Add(this.textBox_ProcessName);
            this.Controls.Add(this.buttonOK);
            this.Name = "AlterGPCDFeedbackProcessForm";
            this.Text = "Lower GPCD based on Deficit Trigger";
            this.toolStrip1.ResumeLayout(false);
            this.toolStrip1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button buttonOK;
        private System.Windows.Forms.TextBox textBox_ProcessName;
        private System.Windows.Forms.Label label_ProcessName;
        private System.Windows.Forms.TextBox textBoxMaxDeficit;
        private System.Windows.Forms.Label labelMaxDeficit;
        private System.Windows.Forms.TextBox textBoxMinGPCD;
        private System.Windows.Forms.Label labelMinGPCD;
        private System.Windows.Forms.ToolStrip toolStrip1;
        private System.Windows.Forms.ToolStripLabel StatusLabel;
    }
}