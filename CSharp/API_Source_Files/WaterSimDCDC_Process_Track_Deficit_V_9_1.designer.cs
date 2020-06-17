namespace WaterSimDCDC.Processes
{
    /// <summary>   Form for viewing the track deficit feedback process. </summary>
    public partial class TrackDeficitFeedbackProcessForm
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
            this.statusStrip1 = new System.Windows.Forms.StatusStrip();
            this.StatusLabel = new System.Windows.Forms.ToolStripStatusLabel();
            this.textBoxMaxDeficit = new System.Windows.Forms.TextBox();
            this.labelMaxDeficit = new System.Windows.Forms.Label();
            this.statusStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // buttonOK
            // 
            this.buttonOK.Location = new System.Drawing.Point(255, 227);
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
            this.textBox_ProcessName.Location = new System.Drawing.Point(125, 12);
            this.textBox_ProcessName.Name = "textBox_ProcessName";
            this.textBox_ProcessName.Size = new System.Drawing.Size(205, 23);
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
            // statusStrip1
            // 
            this.statusStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.StatusLabel});
            this.statusStrip1.Location = new System.Drawing.Point(0, 264);
            this.statusStrip1.Name = "statusStrip1";
            this.statusStrip1.Size = new System.Drawing.Size(342, 22);
            this.statusStrip1.TabIndex = 3;
            this.statusStrip1.Text = "statusStrip1";
            // 
            // StatusLabel
            // 
            this.StatusLabel.Name = "StatusLabel";
            this.StatusLabel.Size = new System.Drawing.Size(0, 17);
            // 
            // textBoxMaxDeficit
            // 
            this.textBoxMaxDeficit.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.textBoxMaxDeficit.Location = new System.Drawing.Point(174, 49);
            this.textBoxMaxDeficit.Name = "textBoxMaxDeficit";
            this.textBoxMaxDeficit.Size = new System.Drawing.Size(156, 23);
            this.textBoxMaxDeficit.TabIndex = 4;
            this.textBoxMaxDeficit.TextChanged += new System.EventHandler(this.textBoxMaxDeficit_TextChanged);
            // 
            // labelMaxDeficit
            // 
            this.labelMaxDeficit.AutoSize = true;
            this.labelMaxDeficit.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.labelMaxDeficit.Location = new System.Drawing.Point(12, 52);
            this.labelMaxDeficit.Name = "labelMaxDeficit";
            this.labelMaxDeficit.Size = new System.Drawing.Size(156, 17);
            this.labelMaxDeficit.TabIndex = 5;
            this.labelMaxDeficit.Text = "Deficit Tracking Trigger";
            // 
            // TrackDeficitFeedbackProcessForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(342, 286);
            this.Controls.Add(this.labelMaxDeficit);
            this.Controls.Add(this.textBoxMaxDeficit);
            this.Controls.Add(this.statusStrip1);
            this.Controls.Add(this.label_ProcessName);
            this.Controls.Add(this.textBox_ProcessName);
            this.Controls.Add(this.buttonOK);
            this.Name = "TrackDeficitFeedbackProcessForm";
            this.Text = "Track Years of Deficit based on Level";
            this.Shown += new System.EventHandler(this.AlterGrowthFeedbackProcessForm_Shown);
            this.statusStrip1.ResumeLayout(false);
            this.statusStrip1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button buttonOK;
        private System.Windows.Forms.TextBox textBox_ProcessName;
        private System.Windows.Forms.Label label_ProcessName;
        private System.Windows.Forms.StatusStrip statusStrip1;
        private System.Windows.Forms.ToolStripStatusLabel StatusLabel;
        private System.Windows.Forms.TextBox textBoxMaxDeficit;
        private System.Windows.Forms.Label labelMaxDeficit;
    }
}