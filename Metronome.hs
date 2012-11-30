<?xml version="1.0" encoding="UTF-8"?>
<interface>
  <!-- interface-requires gtk+ 3.0 -->
  <object class="GtkOffscreenWindow" id="pieces">
    <property name="can_focus">False</property>
    <child>
      <object class="GtkBox" id="box1">
        <property name="visible">True</property>
        <property name="can_focus">False</property>
        <child>
          <object class="GtkLabel" id="track label">
            <property name="visible">True</property>
            <property name="can_focus">False</property>
            <property name="label" translatable="yes">Track</property>
          </object>
          <packing>
            <property name="expand">False</property>
            <property name="fill">True</property>
            <property name="position">0</property>
          </packing>
        </child>
        <child>
          <object class="GtkGrid" id="grid1">
            <property name="visible">True</property>
            <property name="can_focus">False</property>
            <child>
              <object class="GtkLabel" id="label1">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <property name="label" translatable="yes">Numbers</property>
              </object>
              <packing>
                <property name="left_attach">0</property>
                <property name="top_attach">0</property>
                <property name="width">1</property>
                <property name="height">1</property>
              </packing>
            </child>
            <child>
              <object class="GtkLabel" id="label2">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <property name="label" translatable="yes">Synchro</property>
              </object>
              <packing>
                <property name="left_attach">1</property>
                <property name="top_attach">0</property>
                <property name="width">1</property>
                <property name="height">1</property>
              </packing>
            </child>
            <child>
              <object class="GtkLabel" id="label3">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <property name="label" translatable="yes">Offset</property>
              </object>
              <packing>
                <property name="left_attach">2</property>
                <property name="top_attach">0</property>
                <property name="width">1</property>
                <property name="height">1</property>
              </packing>
            </child>
            <child>
              <object class="GtkLabel" id="label4">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <property name="label" translatable="yes">Width</property>
              </object>
              <packing>
                <property name="left_attach">3</property>
                <property name="top_attach">0</property>
                <property name="width">1</property>
                <property name="height">1</property>
              </packing>
            </child>
            <child>
              <object class="GtkSpinButton" id="numbers">
                <property name="visible">True</property>
                <property name="can_focus">True</property>
                <property name="invisible_char">●</property>
              </object>
              <packing>
                <property name="left_attach">0</property>
                <property name="top_attach">1</property>
                <property name="width">1</property>
                <property name="height">1</property>
              </packing>
            </child>
            <child>
              <object class="GtkSpinButton" id="synchro">
                <property name="visible">True</property>
                <property name="can_focus">True</property>
                <property name="invisible_char">●</property>
              </object>
              <packing>
                <property name="left_attach">1</property>
                <property name="top_attach">1</property>
                <property name="width">1</property>
                <property name="height">1</property>
              </packing>
            </child>
            <child>
              <object class="GtkSpinButton" id="offset">
                <property name="visible">True</property>
                <property name="can_focus">True</property>
                <property name="invisible_char">●</property>
              </object>
              <packing>
                <property name="left_attach">2</property>
                <property name="top_attach">1</property>
                <property name="width">1</property>
                <property name="height">1</property>
              </packing>
            </child>
            <child>
              <object class="GtkSpinButton" id="width">
                <property name="visible">True</property>
                <property name="can_focus">True</property>
                <property name="invisible_char">●</property>
              </object>
              <packing>
                <property name="left_attach">3</property>
                <property name="top_attach">1</property>
                <property name="width">1</property>
                <property name="height">1</property>
              </packing>
            </child>
          </object>
          <packing>
            <property name="expand">False</property>
            <property name="fill">True</property>
            <property name="position">1</property>
          </packing>
        </child>
        <child>
          <object class="GtkBox" id="box2">
            <property name="visible">True</property>
            <property name="can_focus">False</property>
            <property name="orientation">vertical</property>
            <child>
              <object class="GtkLabel" id="label6">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <property name="label" translatable="yes">Action</property>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">0</property>
              </packing>
            </child>
            <child>
              <object class="GtkEntry" id="action">
                <property name="visible">True</property>
                <property name="can_focus">True</property>
                <property name="invisible_char">●</property>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">1</property>
              </packing>
            </child>
          </object>
          <packing>
            <property name="expand">True</property>
            <property name="fill">True</property>
            <property name="position">2</property>
          </packing>
        </child>
      </object>
    </child>
  </object>
  <object class="GtkWindow" id="window1">
    <property name="can_focus">False</property>
    <child>
      <object class="GtkBox" id="box3">
        <property name="visible">True</property>
        <property name="can_focus">False</property>
        <property name="orientation">vertical</property>
        <child>
          <object class="GtkBox" id="box4">
            <property name="visible">True</property>
            <property name="can_focus">False</property>
            <property name="orientation">vertical</property>
            <child>
              <object class="GtkBox" id="box5">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <child>
                  <object class="GtkLabel" id="track label1">
                    <property name="width_request">25</property>
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">1</property>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">0</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkGrid" id="grid2">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <child>
                      <object class="GtkLabel" id="label5">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Numbers</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label7">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Synchro</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label8">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Offset</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label9">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Width</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="numbers1">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="synchro1">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="offset1">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="width1">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">1</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="box6">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="orientation">vertical</property>
                    <child>
                      <object class="GtkLabel" id="label10">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Action</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkEntry" id="action1">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">True</property>
                    <property name="fill">True</property>
                    <property name="position">2</property>
                  </packing>
                </child>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">0</property>
              </packing>
            </child>
            <child>
              <object class="GtkBox" id="box7">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <child>
                  <object class="GtkLabel" id="track label2">
                    <property name="width_request">25</property>
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">2</property>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">0</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkGrid" id="grid3">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <child>
                      <object class="GtkLabel" id="label11">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Numbers</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label12">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Synchro</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label13">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Offset</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label14">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Width</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="numbers2">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="synchro2">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="offset2">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="width2">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">1</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="box8">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="orientation">vertical</property>
                    <child>
                      <object class="GtkLabel" id="label15">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Action</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkEntry" id="action2">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">True</property>
                    <property name="fill">True</property>
                    <property name="position">2</property>
                  </packing>
                </child>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">1</property>
              </packing>
            </child>
            <child>
              <object class="GtkBox" id="box9">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <child>
                  <object class="GtkLabel" id="track label3">
                    <property name="width_request">25</property>
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">3</property>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">0</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkGrid" id="grid4">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <child>
                      <object class="GtkLabel" id="label16">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Numbers</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label17">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Synchro</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label18">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Offset</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label19">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Width</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="numbers3">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="synchro3">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="offset3">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="width3">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">1</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="box10">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="orientation">vertical</property>
                    <child>
                      <object class="GtkLabel" id="label20">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Action</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkEntry" id="action3">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">True</property>
                    <property name="fill">True</property>
                    <property name="position">2</property>
                  </packing>
                </child>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">2</property>
              </packing>
            </child>
            <child>
              <object class="GtkBox" id="box11">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <child>
                  <object class="GtkLabel" id="track label4">
                    <property name="width_request">25</property>
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">4</property>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">0</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkGrid" id="grid5">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <child>
                      <object class="GtkLabel" id="label21">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Numbers</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label22">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Synchro</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label23">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Offset</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label24">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Width</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="numbers4">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="synchro4">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="offset4">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="width4">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">1</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="box12">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="orientation">vertical</property>
                    <child>
                      <object class="GtkLabel" id="label25">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Action</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkEntry" id="action4">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">True</property>
                    <property name="fill">True</property>
                    <property name="position">2</property>
                  </packing>
                </child>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">3</property>
              </packing>
            </child>
            <child>
              <object class="GtkBox" id="box13">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <child>
                  <object class="GtkLabel" id="track label5">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">5
</property>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">0</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkGrid" id="grid6">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <child>
                      <object class="GtkLabel" id="label26">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Numbers</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label27">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Synchro</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label28">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Offset</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label29">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Width</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="numbers5">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="synchro5">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="offset5">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="width5">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">1</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="box14">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="orientation">vertical</property>
                    <child>
                      <object class="GtkLabel" id="label30">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Action</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkEntry" id="action5">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">True</property>
                    <property name="fill">True</property>
                    <property name="position">2</property>
                  </packing>
                </child>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">4</property>
              </packing>
            </child>
            <child>
              <object class="GtkBox" id="box15">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <child>
                  <object class="GtkLabel" id="track label6">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">6
</property>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">0</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkGrid" id="grid7">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <child>
                      <object class="GtkLabel" id="label31">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Numbers</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label32">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Synchro</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label33">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Offset</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label34">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Width</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="numbers6">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="synchro6">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="offset6">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="width6">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">1</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="box16">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="orientation">vertical</property>
                    <child>
                      <object class="GtkLabel" id="label35">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Action</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkEntry" id="action6">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">True</property>
                    <property name="fill">True</property>
                    <property name="position">2</property>
                  </packing>
                </child>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">5</property>
              </packing>
            </child>
            <child>
              <object class="GtkBox" id="box17">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <child>
                  <object class="GtkLabel" id="track label7">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">7</property>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">0</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkGrid" id="grid8">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <child>
                      <object class="GtkLabel" id="label36">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Numbers</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label37">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Synchro</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label38">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Offset</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label39">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Width</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="numbers7">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="synchro7">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="offset7">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="width7">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">1</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="box18">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="orientation">vertical</property>
                    <child>
                      <object class="GtkLabel" id="label40">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Action</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkEntry" id="action7">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">True</property>
                    <property name="fill">True</property>
                    <property name="position">2</property>
                  </packing>
                </child>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">6</property>
              </packing>
            </child>
            <child>
              <object class="GtkBox" id="box19">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <child>
                  <object class="GtkLabel" id="track label8">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">8</property>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">0</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkGrid" id="grid9">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <child>
                      <object class="GtkLabel" id="label41">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Numbers</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label42">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Synchro</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label43">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Offset</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label44">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Width</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="numbers8">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="synchro8">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="offset8">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="width8">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">1</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="box20">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="orientation">vertical</property>
                    <child>
                      <object class="GtkLabel" id="label45">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Action</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkEntry" id="action8">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">True</property>
                    <property name="fill">True</property>
                    <property name="position">2</property>
                  </packing>
                </child>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">7</property>
              </packing>
            </child>
            <child>
              <object class="GtkBox" id="box21">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <child>
                  <object class="GtkLabel" id="track label9">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">9</property>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">0</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkGrid" id="grid10">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <child>
                      <object class="GtkLabel" id="label46">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Numbers</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label47">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Synchro</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label48">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Offset</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label49">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Width</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="numbers9">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="synchro9">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="offset9">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="width9">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">1</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="box22">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="orientation">vertical</property>
                    <child>
                      <object class="GtkLabel" id="label50">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Action</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkEntry" id="action9">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">True</property>
                    <property name="fill">True</property>
                    <property name="position">2</property>
                  </packing>
                </child>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">8</property>
              </packing>
            </child>
            <child>
              <object class="GtkBox" id="box23">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <child>
                  <object class="GtkLabel" id="track label10">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">10</property>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">0</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkGrid" id="grid11">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <child>
                      <object class="GtkLabel" id="label51">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Numbers</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label52">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Synchro</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label53">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Offset</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label54">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Width</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="numbers10">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="synchro10">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="offset10">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="width10">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">1</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="box24">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="orientation">vertical</property>
                    <child>
                      <object class="GtkLabel" id="label55">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Action</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkEntry" id="action10">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">True</property>
                    <property name="fill">True</property>
                    <property name="position">2</property>
                  </packing>
                </child>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">9</property>
              </packing>
            </child>
            <child>
              <object class="GtkBox" id="box25">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <child>
                  <object class="GtkLabel" id="track label11">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">11</property>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">0</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkGrid" id="grid12">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <child>
                      <object class="GtkLabel" id="label56">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Numbers</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label57">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Synchro</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label58">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Offset</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label59">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Width</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="numbers11">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="synchro11">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="offset11">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="width11">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">1</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="box26">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="orientation">vertical</property>
                    <child>
                      <object class="GtkLabel" id="label60">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Action</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkEntry" id="action11">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">True</property>
                    <property name="fill">True</property>
                    <property name="position">2</property>
                  </packing>
                </child>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">10</property>
              </packing>
            </child>
            <child>
              <object class="GtkBox" id="box27">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <child>
                  <object class="GtkLabel" id="track label12">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">12</property>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">0</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkGrid" id="grid13">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <child>
                      <object class="GtkLabel" id="label61">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Numbers</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label62">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Synchro</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label63">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Offset</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="label64">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Width</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">0</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="numbers12">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">0</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="synchro12">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">1</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="offset12">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">2</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkSpinButton" id="width12">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="left_attach">3</property>
                        <property name="top_attach">1</property>
                        <property name="width">1</property>
                        <property name="height">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">1</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="box28">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="orientation">vertical</property>
                    <child>
                      <object class="GtkLabel" id="label65">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="label" translatable="yes">Action</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkEntry" id="action12">
                        <property name="visible">True</property>
                        <property name="can_focus">True</property>
                        <property name="invisible_char">●</property>
                        <property name="invisible_char_set">True</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="expand">True</property>
                    <property name="fill">True</property>
                    <property name="position">2</property>
                  </packing>
                </child>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">11</property>
              </packing>
            </child>
            <child>
              <placeholder/>
            </child>
            <child>
              <placeholder/>
            </child>
            <child>
              <placeholder/>
            </child>
            <child>
              <placeholder/>
            </child>
          </object>
          <packing>
            <property name="expand">False</property>
            <property name="fill">True</property>
            <property name="position">0</property>
          </packing>
        </child>
        <child>
          <object class="GtkButtonBox" id="buttonbox1">
            <property name="visible">True</property>
            <property name="can_focus">False</property>
            <property name="layout_style">start</property>
            <child>
              <placeholder/>
            </child>
            <child>
              <placeholder/>
            </child>
            <child>
              <placeholder/>
            </child>
            <child>
              <placeholder/>
            </child>
            <child>
              <placeholder/>
            </child>
            <child>
              <placeholder/>
            </child>
            <child>
              <placeholder/>
            </child>
            <child>
              <placeholder/>
            </child>
            <child>
              <placeholder/>
            </child>
            <child>
              <placeholder/>
            </child>
          </object>
          <packing>
            <property name="expand">False</property>
            <property name="fill">True</property>
            <property name="position">1</property>
          </packing>
        </child>
      </object>
    </child>
  </object>
</interface>
