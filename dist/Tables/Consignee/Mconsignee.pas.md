<!-- tabs:start -->

#### **Documentation**

# Documentation for `Mconsignee` Code Unit

## 1. Overview:

### Objective:
The `Mconsignee` code unit is designed to manage and maintain consignee-related data within a user interface. It provides a form-based interface for users to view, edit, and manage consignee details, including associated data such as addresses, contacts, delivery days, vehicle types, and more. The form also supports actions like duplicating records, printing current records, and navigating through detailed tabs for specific consignee-related information.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the graphical user interface and handling events.
- **Third-party Components**: Includes components like `TsPanel`, `TsSplitter`, `TsBitBtn`, and `TsPageControl` for enhanced UI/UX.
- **Database Interaction**: Uses `DBClient` for managing data connections and operations.
- **Custom Frames**: Includes custom frames like `FRAMEconsignee`, `FRAMElistAddresses`, and others for modular UI design.

### Form Type:
This is a **form-based interface** with the following elements:
- **Form Elements**:
  - Panels (`TsPanel`)
  - Buttons (`TsBitBtn`, `TsSpeedButton`)
  - Splitters (`TsSplitter`)
  - Tab Control (`TsPageControl` with multiple tabs)
  - Custom Frames for specific functionalities
- **Form Actions**:
  - Duplicate a record
  - Print the current record
  - Add new records
  - Cancel operations
  - Navigate through tabs to view/edit specific details

---

## 2. Functionality Description:

### User/Software Actions:
- View and edit consignee details.
- Duplicate existing consignee records.
- Print the current record.
- Add new consignee-related data (e.g., delivery days, addresses).
- Navigate through tabs to manage specific consignee-related information.

### Main Components:
1. **Panels (`TsPanel`)**: Organize the layout and group related controls.
2. **Buttons (`TsBitBtn`, `TsSpeedButton`)**: Trigger actions like duplication, printing, and navigation.
3. **Tab Control (`TsPageControl`)**: Provides a tabbed interface for managing different aspects of consignee data.
4. **Custom Frames**: Modular components for specific functionalities (e.g., `FRAMEconsignee`, `FRAMElistAddresses`).

### Pseudo-code for Actions and Events:
- `OnClick` event of `BTprintCurrentRecord`: `if button clicked then print current record`.
- `OnClick` event of `FRAMEconsDelDay1BTNadd`: `if button clicked then add new delivery day`.
- `OnExecute` event of `ACTduplicate`: `if action executed then duplicate current record`.
- `OnClick` event of `BTNew`: `if button clicked then create new record`.
- `OnTimer` event of `TMRstdLogCost`: `if timer triggered then execute standard logistic cost logic`.
- `OnClick` event of `BTCancel`: `if button clicked then cancel operation`.
- `OnChange` event of `PGCdetails`: `if tab changed then load corresponding data`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is initialized, and UI components are loaded.
   - Data is fetched and displayed in the respective fields and tabs.

2. **User Interactions**:
   - Users can interact with buttons, tabs, and other controls to perform actions like duplication, printing, and data entry.

3. **Triggered Functions**:
   - `FormShow`: Initializes the form and loads data.
   - `BTprintCurrentRecordClick`: Prints the current record.
   - `FRAMEconsDelDay1BTNaddClick`: Adds a new delivery day.
   - `ACTduplicateExecute`: Duplicates the current record.
   - `BTNewClick`: Creates a new record.
   - `TMRstdLogCostTimer`: Handles timer-based logic for standard logistic costs.
   - `BTCancelClick`: Cancels the current operation.
   - `PGCdetailsChange`: Handles tab change events.

### Required Data:
- Consignee details (e.g., name, address, contact information).
- Delivery day information.
- Vehicle type and special device details.
- Logistic cost data.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Duplicate Button**: Enabled only if the user has the necessary permissions (`FcanDuplicate`).
- **Print Button**: Enabled when a record is selected.
- **Add Button**: Enabled when the user is on the delivery day tab.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- Not explicitly defined in the code.

---

## 5. Main Functions:

1. **`m_getData`**: Fetches data to populate the form.
2. **`m_Validate`**: Validates the form data before saving.
3. **`m_PutData`**: Saves the form data to the database.
4. **`m_CreateFormEdit`**: Creates and initializes the form.
5. **`SetFormState`**: Configures the form's state based on access mode.
6. **`SetFrameState`**: Configures the state of specific frames.

---

## 6. API Service Consumption:

No explicit API calls are defined in the code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **Delphi VCL Components**: For UI and event handling.
- **Third-party Components**: Includes `TsPanel`, `TsSplitter`, `TsBitBtn`, etc.

### Custom Components:
- **Custom Frames**: `FRAMEconsignee`, `FRAMElistAddresses`, `FRAMEconsDelDay`, etc.

---

## 9. Fields and Validations Listing:

### Fields:
- **AddressMasterKeyFields**: Type: String, Purpose: Links addresses to entities.
- **FcanDuplicate**: Type: Boolean, Purpose: Determines if the user can duplicate records.

### Mapping:
- Not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Form Initialization] --> [Load Data] --> [User Interactions] --> [Perform Actions] --> [Save/Print/Cancel] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Form: Open Form
User --> Button: Click Action
Form --> Function: Execute Corresponding Logic
Function --> Database: Fetch/Save Data
Database --> Form: Return Data
```

### Code Snippets:
```delphi
procedure TFORMMconsignee.BTprintCurrentRecordClick(Sender: TObject);
begin
  // Logic to print the current record
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- `FAddressMasterKeyFields`: Used to define the link with addresses, as they are used by multiple entities.
- `FcanDuplicate`: Stores whether the user has permission to duplicate records.

---

## 12. Conclusion:

The `Mconsignee` code unit provides a comprehensive interface for managing consignee-related data. It is modular, with custom frames for specific functionalities, and supports essential actions like duplication and printing. However, the code lacks explicit error handling, field validations, and default values, which could be improved.

---

## 13. Short Summary:

The `Mconsignee` code unit is a form-based interface for managing consignee data, supporting actions like duplication, printing, and detailed data management through tabs. It is modular and extensible but lacks explicit error handling and field validations.#### **Mconsignee.pas**

```
unit Mconsignee;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBedit, StdCtrls, Buttons, ExtCtrls, ComCtrls, kneFREditSOA, FRconsignee,
  kneFRGridEditSOA, FRconsVehType, FRconsRpPosit,
  FRconsSplDevice, FRconsDelDay, kneFRCtrlEditSOA, DBClient,
  FRconsBook, knePrivileges, sPageControl, sSpeedButton, sBitBtn, ToolWin,
  acCoolBar, sPanel, ImgList, kneEnterAsTab, ActnList, sSplitter,
  FRlistContacts, FRlistAddresses, FRconsMill, FRdocumentsInformation,
  FRconsStandardLogisticCosts, FRextShipDelCons, FRentityComm;

type
  TFORMMconsignee = class(TFORMkneBaseEdit)
    PNLconsignee: TsPanel;
    FRAMEconsignee1: TFRAMEconsignee;
    sSplitter1: TsSplitter;
    PGCdetails: TsPageControl;
    TSHbooking: TsTabSheet;
    FRAMEconsBook1: TFRAMEconsBook;
    TSHweekDelDay: TsTabSheet;
    FRAMEconsDelDay1: TFRAMEconsDelDay;
    TSHallowVehicle: TsTabSheet;
    FRAMEconsVehType1: TFRAMEconsVehType;
    TSHspecialDevice: TsTabSheet;
    FRAMEconsSplDevice1: TFRAMEconsSplDevice;
    TSHunitsPositioning: TsTabSheet;
    FRAMEconsRpPosit1: TFRAMEconsRpPosit;
    SHaddresses: TsTabSheet;
    FRAMElistAddresses1: TFRAMElistAddresses;
    SPL2: TsSplitter;
    FRAMElistContacts1: TFRAMElistContacts;
    TSHmill: TsTabSheet;
    FRAMEconsMill1: TFRAMEconsMill;
    TSHdocs: TsTabSheet;
    FRAMEdocumentsInformation1: TFRAMEdocumentsInformation;
    PNL1: TsPanel;
    BTNaddress: TsBitBtn;
    ACTduplicate: TAction;
    TSHstdLogisticCost: TsTabSheet;
    FRAMEconsStandardLogisticCosts1: TFRAMEconsStandardLogisticCosts;
    TMRstdLogCost: TTimer;
    TSHextShipDelCons: TsTabSheet;
    FRAMEextShipDelCons1: TFRAMEextShipDelCons;
    TSHconsComm: TsTabSheet;
    FRAMEentityComm1: TFRAMEentityComm;
    procedure FormShow(Sender: TObject);
    procedure BTprintCurrentRecordClick(Sender: TObject);
    procedure FRAMEconsDelDay1BTNaddClick(Sender: TObject);
    procedure ACTduplicateExecute(Sender: TObject);
    procedure BTNewClick(Sender: TObject);
    procedure TMRstdLogCostTimer(Sender: TObject);
    procedure BTCancelClick(Sender: TObject);
    procedure PGCdetailsChange(Sender: TObject);
  private
    { Private declarations }
    FAddressMasterKeyFields: String;   // � necess�ria para definir a liga��o com os address ( uma vez que estes s�o utilizados por varias entidades)
    FcanDuplicate: Boolean; //JAR #5354  18-02-2010  Suporte para o Duplicate, guarda se o utilizador tem permiss�o para efectuar o duplicate

    function GetAddrMasterKeyFields: String;
    procedure m_SetContactsDataSet(sender: TObject);
    procedure DoProtectControl(Sender: TObject;
      const pv_Args: array of const);
    function SetAllControlsState(const pv_Control: TControl;
      const pv_Enabled: Boolean): Boolean;
    procedure SetProtectFieldsState(Sender: TFRAMEBaseGridEditSOA);
    procedure SetFormState(pv_AccessMode: string);
    procedure SetFrameState(pv_Frame: TFRAMEBaseGridEditSOA;
      pv_CanEdit: Boolean);                               overload;
    procedure SetFrameState(pv_Frame: TFRAMEBaseCtrlEditSOA;
  pv_CanEdit: Boolean; pv_Fields : array of TClass);       overload;
    function m_ConsInfoIsSimilar(pv_Frame: TFRAMEconsignee): Boolean;

  protected
    procedure m_getData; override;
    function m_Validate: Boolean; override;
    procedure m_PutData; override;

  public
    { Public declarations }
    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
    constructor Create(AOwner: TComponent);
  published
    property AddressMasterKeyFields: string read GetAddrMasterKeyFields;// � necess�ria para definir a liga��o com os address ( uma vez que estes s�o utilizados por varias entidades)
  end;

var
  FORMMconsignee: TFORMMconsignee;

implementation

uses  
  kneUtils, Global, kneFRFindEditSOA,
  ConsigneeServiceUtils {NAVOPTECH2022-4055},
  //---
  DRentities;

{$R *.dfm}
```

#### **Mconsignee.dfm**

```
inherited FORMMconsignee: TFORMMconsignee
  Left = 466
  Top = 87
  Width = 975
  Height = 788
  Caption = 'Consignee Maintenance'
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited PNLtoolbar: TsPanel
    Width = 959
    inherited CLBactions: TsCoolBar
      Width = 959
      Bands = <
        item
          Control = PNbotoes
          ImageIndex = -1
          MinHeight = 41
          Width = 955
        end>
      inherited PNbotoes: TsPanel
        Width = 942
        inherited BTNseparator2: TsSpeedButton
          Left = 640
        end
        inherited PNLprint: TsPanel
          Visible = True
          inherited BTprintCurrentRecord: TsBitBtn
            OnClick = BTprintCurrentRecordClick
          end
        end
        object PNL1: TsPanel
          Left = 552
          Top = 1
          Width = 88
          Height = 39
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 7
          SkinData.SkinSection = 'ALPHACOMBOBOX'
          object BTNaddress: TsBitBtn
            Left = 3
            Top = 2
            Width = 82
            Height = 30
            Action = ACTduplicate
            Caption = 'Duplicate'
            TabOrder = 0
            TabStop = False
            SkinData.SkinSection = 'SPEEDBUTTON'
            ImageIndex = 10
            Images = IMLbuttons
          end
        end
      end
    end
  end
  object PNLconsignee: TsPanel [2]
    Left = 0
    Top = 41
    Width = 959
    Height = 709
    Align = alClient
    TabOrder = 1
    SkinData.SkinSection = 'PANEL'
    object sSplitter1: TsSplitter
      Left = 1
      Top = 312
      Width = 957
      Height = 4
      Cursor = crVSplit
      Align = alTop
      SkinData.SkinSection = 'FORM'
    end
    inline FRAMEconsignee1: TFRAMEconsignee
      Left = 1
      Top = 1
      Width = 957
      Height = 311
      Align = alTop
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      inherited LBLcountry: TsLabel
        FocusControl = FRAMEconsignee1.FRAMEfindCountry.DBE
      end
      inherited LBLstate: TsLabel
        FocusControl = FRAMEconsignee1.FRAMEfindOrdDest.DBE
      end
      inherited LBLmarket: TsLabel
        FocusControl = FRAMEconsignee1.FRAMEfindConsMarket.DBE
      end
      inherited LBLwarehouse: TsLabel
```
<!-- tabs:end -->

