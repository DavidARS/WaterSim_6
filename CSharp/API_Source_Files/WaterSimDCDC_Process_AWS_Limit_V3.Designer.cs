namespace WaterSimDCDC.Processes
{
    partial class AWSFeedbackProcessForm
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
            this.AssuredTriggertextBox = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
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
            this.textBox_ProcessName.Location = new System.Drawing.Point(98, 12);
            this.textBox_ProcessName.Name = "textBox_ProcessName";
            this.textBox_ProcessName.Size = new System.Drawing.Size(232, 20);
            this.textBox_ProcessName.TabIndex = 1;
            this.textBox_ProcessName.TextChanged += new System.EventHandler(this.textBox_ProcessName_TextChanged);
            // 
            // label_ProcessName
            // 
            this.label_ProcessName.AutoSize = true;
            this.label_ProcessName.Location = new System.Drawing.Point(16, 15);
            this.label_ProcessName.Name = "label_ProcessName";
            this.label_ProcessName.Size = new System.Drawing.Size(76, 13);
            this.label_ProcessName.TabIndex = 2;
            this.label_ProcessName.Text = "Process Name";
            // 
            // statusStrip1
            // 
            this.statusStrip1.Location = new System.Drawing.Point(0, 264);
            this.statusStrip1.Name = "statusStrip1";
            this.statusStrip1.Size = new System.Drawing.Size(342, 22);
            this.statusStrip1.TabIndex = 3;
            this.statusStrip1.Text = "statusStrip1";
            // 
            // AssuredTriggertextBox
            // 
            this.AssuredTriggertextBox.Location = new System.Drawing.Point(230, 53);
            this.AssuredTriggertextBox.Name = "AssuredTriggertextBox";
            this.AssuredTriggertextBox.Size = new System.Drawing.Size(100, 20);
            this.AssuredTriggertextBox.TabIndex = 4;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(16, 56);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(181, 13);
            this.label1.TabIndex = 5;
            this.label1.Text = "Years of Unassurable GW for Trigger";
            // 
            // AWSFeedbackProcessForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(342, 286);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.AssuredTriggertextBox);
            this.Controls.Add(this.statusStrip1);
            this.Controls.Add(this.label_ProcessName);
            this.Controls.Add(this.textBox_ProcessName);
            this.Controls.Add(this.buttonOK);
            this.Name = "AWSFeedbackProcessForm";
            this.Text = "Base Process Form";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button buttonOK;
        private System.Windows.Forms.TextBox textBox_ProcessName;
        private System.Windows.Forms.Label label_ProcessName;
        private System.Windows.Forms.StatusStrip statusStrip1;
        private System.Windows.Forms.TextBox AssuredTriggertextBox;
        private System.Windows.Forms.Label label1;
    }
}