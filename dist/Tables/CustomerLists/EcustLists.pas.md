<!-- tabs:start -->

#### **Documentation**

# Documentation for `EcustLists` Code Unit

## 1. Overview:

### Objective:
The `EcustLists` code unit is designed to manage and display customer lists in a structured and interactive form. It provides functionality for viewing, editing, and managing customer-related data, including master-detail relationships. The form also includes mechanisms to handle changes in market selection and ensures data integrity by prompting users before performing critical actions like deleting customer codes.

### Technologies Used:
- **Delphi VCL Framework**: Used for building the graphical user interface and handling events.
- **Third-party Components**: Includes components like `TsPanel`, `TcxGrid`, and `TsDBComboBox` for enhanced UI and data handling.
- **Database Integration**: Uses data sources (`DStable`, `CDStable`) for managing and displaying database records.

### Form Type:
This is a **form** with the following elements:
- **Form Elements**:
  - `FRAMEcustLists1`: A frame for managing customer lists.
  - `FRAMEcustListsDetail1`: A frame for displaying detailed customer data in a grid.
  - `PNLdata`: A panel for organizing the layout.
  - `IMLbuttons`: An image list for button icons.
- **Form Actions**:
  - **Market Change**: Prompts the user and deletes customer codes if the market is changed.
  - **Cancel Button**: Cancels the current operation.

---

## 2. Functionality Description:

### User/Software Actions:
- View and edit customer lists.
- Change the market selection and handle related data updates.
- Delete customer codes after user confirmation.

### Main Components:
- **`FRAMEcustLists1`**: Handles market selection and displays customer list information.
- **`FRAMEcustListsDetail1`**: Displays detailed customer data in a grid format.
- **`PNLdata`**: Organizes the layout of the form.

### Pseudo-code for Actions and Events:
- **OnClick event of Cancel Button**:  
  `if cancel button clicked then close form`.
- **OnChange event of Market Field**:  
  `if market value changed then prompt user for confirmation`.  
  `if user confirms then delete all customer codes`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**: The form is created using the `m_CreateFormEdit` method, which initializes the components and sets up the master-detail relationship.
2. **Data Loading**: The `m_getData` method loads data into the form and sets up event handlers.
3. **User Interaction**:
   - Changing the market triggers the `m_OnChangeMkt` method, which prompts the user and deletes customer codes if confirmed.
   - Clicking the cancel button closes the form.

### Required User Data:
- Market selection in `FRAMEcustLists1`.
- Customer details in `FRAMEcustListsDetail1`.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Market Change**:
  - Preconditions: A market must be selected.
  - Action: Prompts the user and deletes customer codes if confirmed.
- **Cancel Button**:
  - Preconditions: None.
  - Action: Closes the form.

### Available Filters:
- Market selection in `FRAMEcustLists1`.

### Error Messages:
- "Cust codes will be deleted. Are you Sure?" if the market is changed.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- Market field: Must match the format expected by the system.

---

## 5. Main Functions:

### Functions:
1. **`m_CreateFormEdit`**:
   - Creates and initializes the form.
2. **`m_getData`**:
   - Loads data into the form and sets up event handlers.
3. **`m_OnChangeMkt`**:
   - Handles market changes and deletes customer codes after user confirmation.

---

## 6. API Service Consumption:

No external API services are consumed in this code.

---

## 7. Conditional Fields (Form Logic):

- The market field (`FRAMEFindMarket`) triggers conditional logic:
  - If the market value changes, the system prompts the user and deletes customer codes if confirmed.

---

## 8. Dependencies:

### External Libraries:
- **`TsPanel`, `TcxGrid`, `TsDBComboBox`**: Used for UI components.
- **`kneUtils`**: Provides utility functions.

### Custom Components:
- **`FRAMEcustLists1`**: Custom frame for managing customer lists.
- **`FRAMEcustListsDetail1`**: Custom frame for displaying detailed customer data.

---

## 9. Fields and Validations Listing:

### Fields:
1. **Market Field**:
   - Type: String.
   - Required: Yes.
   - Validation: Must match the expected format.
2. **Customer Codes**:
   - Type: String.
   - Required: Yes.
   - Validation: Not explicitly defined in the code.

### Mapping:
- Displayed values are mapped to database columns via `DStable` and `CDStable`.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Form] --> [Load Data] --> [User Interaction]
    --> [Market Change?] --> [Prompt User] --> [Delete Customer Codes]
    --> [End]
```

### Sequence Diagram:
```plaintext
User --> Form: Change Market
Form --> User: Prompt Confirmation
User --> Form: Confirm
Form --> Database: Delete Customer Codes
```

### Code Snippets:
```delphi
procedure TFORMEcustLists.m_OnChangeMkt(Sender: TObject);
begin
  if not SameText(FRAMEcustLists1.FRAMEFindMarket.Text, FMktValue) then
  begin
    if MessageDlg('Cust codes will be deleted. Are you Sure?', mtWarning, [mbYes, mbNo], 0) = mrYes then
    begin
      CDStable.DisableControls;
      try
        while not CDStable.Eof do
          CDStable.Delete;
      finally
        CDStable.EnableControls;
      end;
    end;
  end;
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- **`//NAVOPTECH2022-4707 (cmosilva 20-07-2023)`**: Indicates a specific change or feature related to market change handling.

---

## 12. Conclusion:

The `EcustLists` code unit provides a robust solution for managing customer lists with master-detail relationships. It ensures data integrity through user prompts and handles market changes effectively. However, the code lacks explicit field validations and default values, which could be improved for better usability.

---

## 13. Short Summary:

The `EcustLists` code unit manages customer lists with master-detail relationships, handles market changes with user prompts, and ensures data integrity. It is a well-structured form but could benefit from more explicit field validations and default values.#### **EcustLists.pas**

```
unit EcustLists;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBEdit, ActnList, ImgList, knePrivileges, StdCtrls, Buttons,
  sBitBtn, sSpeedButton, ToolWin, ComCtrls, acCoolBar, ExtCtrls, sPanel,
  kneEnterAsTab, kneFRGridEditSOA, FRcustListsDetail, kneFREditSOA,
  kneFRCtrlEditSOA, FRcustLists;

type
  TFORMEcustLists = class(TFORMkneBaseEdit)
    PNLdata: TsPanel;
    FRAMEcustLists1: TFRAMEcustLists;
    FRAMEcustListsDetail1: TFRAMEcustListsDetail;
    procedure BTCancelClick(Sender: TObject);

  protected
    procedure m_getData; override;

  private
    { Private declarations }
    FMktValue: String;
    procedure m_OnChangeMkt(Sender: TObject);
  public
    { Public declarations }
    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
  end;

var
  FORMEcustLists: TFORMEcustLists;

implementation

uses
  kneUtils;

{$R *.dfm}

{ TFORMEcustLists }

class function TFORMEcustLists.m_CreateFormEdit(
  const AOwner: TComponent): TFORMkneBaseEdit;
begin
  Result := TFORMEcustLists.Create(Application);
end;

procedure TFORMEcustLists.m_getData;
var
  lv_MasterFrame: TFRAMEBaseEditSOA;
begin
  Screen.Cursor := crHourGlass;

  lv_MasterFrame := TFRAMEBaseEditSOA(kneUtils.TkneGeneric.fg_GetMasterFrame(Self));
  // setup das rela��es master-detail
  FRAMEcustListsDetail1.MasterSource := lv_MasterFrame.DStable;

  inherited m_getData;

  FRAMEcustLists1.OnChangeMkt := m_OnChangeMkt;
  FMktValue := FRAMEcustLists1.FRAMEFindMarket.Text;

end;

//NAVOPTECH2022-4707 (cmosilva 20-07-2023)
procedure TFORMEcustLists.m_OnChangeMkt(Sender: TObject);
begin

  with FRAMEcustListsDetail1 do
  begin

    if not CDStable.IsEmpty then
    begin
      if not SameText(FRAMEcustLists1.FRAMEFindMarket.Text, FMktValue) then
      begin
        if MessageDlg('Cust codes will be deleted. Are you Sure?', mtWarning, [mbYes, mbNo], 0) = mrYes then
        begin

            if (not CDStable.Active) or (CDStable.IsEmpty) then Exit;

            CDStable.DisableControls;
            try
              CDStable.First;

              while not CDStable.Eof do
              begin
                CDStable.Delete;

              end; //while

            finally
              if CDStable.ControlsDisabled then
                CDStable.EnableControls;
            end;

        end else
        begin

          FRAMEcustLists1.FRAMEFindMarket.SetValue(FMktValue);
```

#### **EcustLists.dfm**

```
inherited FORMEcustLists: TFORMEcustLists
  Left = 640
  Top = 323
  Height = 562
  Caption = 'Customer Lists'
  Font.Name = 'Verdana'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object PNLdata: TsPanel [2]
    Left = 0
    Top = 41
    Width = 784
    Height = 483
    Align = alClient
    TabOrder = 1
    SkinData.SkinSection = 'PANEL'
    inline FRAMEcustLists1: TFRAMEcustLists
      Left = 1
      Top = 1
      Width = 782
      Height = 155
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      inherited LBL2: TsLabel
        FocusControl = FRAMEcustLists1.FRAMEFindMarket.DBE
      end
      inherited LBLbusUnit: TsLabel
        FocusControl = FRAMEcustLists1.FRAMEBusUnit1.DBCBObusUnit
      end
      inherited PNLfooter: TsPanel
        Top = 121
        Width = 782
      end
      inherited FRAMEstatusInfo1: TFRAMEstatusInfo
        Top = 79
        Width = 782
        inherited GRPstatus: TsGroupBox
          Width = 782
          inherited DBTXTlastUpd: TsDBText
            DataSource = FRAMEcustLists1.DStable
          end
          inherited DBTXTupdBy: TsDBText
            DataSource = FRAMEcustLists1.DStable
          end
          inherited ICBOstat: TcxDBImageComboBox
            DataBinding.DataSource = FRAMEcustLists1.DStable
            Width = 97
          end
        end
      end
      inherited FRAMEBusUnit1: TFRAMEBusUnit
        inherited DBCBObusUnit: TsDBComboBox
          DataSource = FRAMEcustLists1.DStable
        end
      end
    end
    inline FRAMEcustListsDetail1: TFRAMEcustListsDetail
      Left = 1
      Top = 156
      Width = 782
      Height = 326
      Align = alClient
      ParentBackground = False
      TabOrder = 1
      inherited cxDBG: TcxGrid
        Width = 782
        Height = 292
      end
      inherited PNLfooter: TsPanel
        Top = 292
        Width = 782
      end
    end
  end
  inherited IMLbuttons: TImageList
    Bitmap = {
      494C01010C000E00040018001800FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000600000006000000001002000000000000090
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
```
<!-- tabs:end -->

