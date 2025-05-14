<!-- tabs:start -->

#### **Documentation**

# Documentation for `Mcarrier` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `Mcarrier` code unit is designed to manage carrier-related data within an application. It provides a user interface for managing carriers, their associated addresses, contacts, available vehicles, and other related information. The form allows users to view, edit, and interact with carrier data, ensuring efficient management of carrier records.

### Technologies Used:
- **Delphi**: The code is written in Delphi, utilizing its VCL (Visual Component Library) for UI components.
- **Custom Components**: Includes custom frames and panels such as `TFRAMEcarrier`, `TFRAMEcarrierAvailableVehicle`, `TFRAMElistAddresses`, and others.
- **Database Integration**: Uses `DBClient` for database operations and data binding.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - `TsPanel`: Used for grouping and organizing UI components.
  - `TsSplitter`: Allows resizing of sections.
  - `TsPageControl` and `TsTabSheet`: For tabbed navigation.
  - `TsBitBtn`: Button for specific actions (e.g., "SAP Code").
  - `TFRAMEcarrier`, `TFRAMElistAddresses`, etc.: Custom frames for specific functionalities.
- **Form Actions and Effects**:
  - Button clicks trigger actions like opening forms, executing commands, or interacting with the database.
  - Tab changes update the displayed data.

---

## 2. Functionality Description:

### User/Software Actions:
- View and manage carrier details.
- Add or edit available vehicles for a carrier.
- Manage associated addresses and contacts.
- Interact with SAP codes for carriers.

### Main Components:
- **`TFRAMEcarrier`**: Displays and manages carrier details.
- **`TFRAMEcarrierAvailableVehicle`**: Manages available vehicles for the carrier.
- **`TFRAMElistAddresses`**: Displays and manages addresses associated with the carrier.
- **`TFRAMElistContacts`**: Displays and manages contacts associated with the carrier.
- **`TsPageControl`**: Provides tabbed navigation for different sections (addresses, mill, carrier type).

### Pseudo-code for Actions and Events:
- `OnClick` event of `BTNsapCode`: `if button clicked then execute ACTsapCode`.
- `OnClick` event of `FRAMEcarrierAvailableVehicle1BTNadd`: `if button clicked then add new vehicle`.
- `OnChange` event of `PGCaddress`: `if tab changed then update displayed data`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is created using `m_CreateFormEdit`.
   - Data is loaded using `m_getData`.
2. **User Interactions**:
   - Clicking buttons triggers specific actions (e.g., opening forms, executing commands).
   - Changing tabs updates the displayed data.
3. **Functions**:
   - `m_CreateFormEdit` (File: `Mcarrier`): Creates the form instance.
   - `m_getData` (File: `Mcarrier`): Loads data and sets up data bindings.
   - `ACTsapCodeExecute` (File: `Mcarrier`): Executes the SAP code action.

### Required User Data:
- Carrier details (e.g., name, type).
- Address and contact information.
- Vehicle details (if applicable).

---

## 4. Business Rules:

### Actions and Preconditions:
- **"SAP Code" Button**: Enabled only when specific conditions are met (e.g., valid carrier data).
- **Add Vehicle Button**: Requires a valid carrier to be selected.

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

- **`m_CreateFormEdit`**: Creates and initializes the form.
- **`m_getData`**: Loads data and sets up data bindings for the form and its components.
- **`ACTsapCodeExecute`**: Handles the execution of the SAP code action.
- **`m_SetContactsDataSet`**: Sets the dataset for contacts.
- **`m_HasSapCodeOnMill`**: Checks if a SAP code exists for the mill.
- **`m_OpenSapCodeForm`**: Opens the SAP code form.

---

## 6. API Service Consumption:

No explicit API calls are defined in the provided code.

---

## 7. Conditional Fields (Form Logic):

- The "SAP Code" button (`BTNsapCode`) is only enabled when specific conditions are met (e.g., valid carrier data).

---

## 8. Dependencies:

### External Libraries:
- **Delphi VCL Components**: Used for UI and database operations.
- **Custom Components**: Includes `TFRAMEcarrier`, `TFRAMElistAddresses`, `TFRAMEcarrierAvailableVehicle`, etc.

### Custom Components:
- **`TFRAMEcarrier`**: Manages carrier details.
- **`TFRAMElistAddresses`**: Manages addresses.
- **`TFRAMElistContacts`**: Manages contacts.
- **`TFRAMEcarrierAvailableVehicle`**: Manages available vehicles.

---

## 9. Fields and Validations Listing:

- **Carrier Details**: Managed by `TFRAMEcarrier`.
- **Addresses**: Managed by `TFRAMElistAddresses`.
- **Contacts**: Managed by `TFRAMElistContacts`.
- **Available Vehicles**: Managed by `TFRAMEcarrierAvailableVehicle`.

Field constraints and validations are not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable due to the lack of detailed workflow in the code.

### Sequence Diagram:
Not applicable due to the lack of detailed interactions in the code.

### Code Snippets:
Example of creating the form:
```delphi
var
  CarrierForm: TFORMMcarrier;
begin
  CarrierForm := TFORMMcarrier.m_CreateFormEdit(Application);
  CarrierForm.Show;
end;
```

### Screenshots:
The DFM file is partially provided. Below is an HTML representation of the form layout:

```html
<div style="width: 830px; height: 525px; border: 1px solid black;">
  <div style="height: 41px; background-color: #f0f0f0;">Toolbar</div>
  <div style="display: flex; height: 484px;">
    <div style="width: 50%; border-right: 1px solid black;">Carrier Details</div>
    <div style="width: 50%;">Additional Information</div>
  </div>
</div>
```

---

## 11. Important Comments in the Code:

- `// Substituir pelo nome do form`: Indicates where the form name should be replaced.
- `// # 3467`: Marks a specific functionality related to `FRAMEcarrierMill1`.
- `//NAVOPTECH2022-875`: Marks a specific change or feature implementation.

---

## 12. Conclusion:

The `Mcarrier` code unit provides a comprehensive interface for managing carrier-related data. Its modular design, using custom frames and components, ensures flexibility and reusability. However, the lack of explicit field validations, error messages, and API integrations limits its robustness.

---

## 13. Short Summary:

The `Mcarrier` code unit is a Delphi-based form for managing carrier data, including addresses, contacts, and vehicles. It uses custom components for modularity and supports SAP code integration. However, it lacks explicit validations and error handling.#### **Mcarrier.pas**

```
unit Mcarrier;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBedit, StdCtrls, Buttons, ExtCtrls, kneFREditSOA,
  kneFRGridEditSOA, FRcarrier, FRcarrierAvailableVehicle, kneFRCtrlEditSOA,
  DBClient, knePrivileges, ImgList, sSpeedButton, sBitBtn, ToolWin,
  ComCtrls, acCoolBar, sPanel, kneEnterAsTab, sPageControl, ActnList,
  sSplitter, FRlistContacts, FRlistAddresses, FRcarrierMill, FRcarrierType,
  DsapCode{#NAVOPTECH2022-785};

type
  TFORMMcarrier = class(TFORMkneBaseEdit)
    PNLcarrier: TsPanel;
    FRAMEcarrier1: TFRAMEcarrier;
    sSplitter1: TsSplitter;
    PGCaddress: TsPageControl;
    TSHaddresses: TsTabSheet;
    SH1: TsTabSheet;
    FRAMEcarrierAvailableVehicle1: TFRAMEcarrierAvailableVehicle;
    FRAMElistAddresses1: TFRAMElistAddresses;
    SPL2: TsSplitter;
    FRAMElistContacts1: TFRAMElistContacts;
    TSHmill: TsTabSheet;
    TSHcarrierType: TsTabSheet;
    FRAMEcarrierType1: TFRAMEcarrierType;
    PNLsapCode: TsPanel;
    BTNsapCode: TsBitBtn;
    ACTsapCode: TAction;
    FRAMEcarrierMill1: TFRAMEcarrierMill;
    procedure BTprintCurrentRecordClick(Sender: TObject);
    procedure FRAMEcarrierAvailableVehicle1BTNaddClick(Sender: TObject);
    procedure ACTsapCodeExecute(Sender: TObject);
    procedure PGCaddressChange(Sender: TObject);
    procedure BTCancelClick(Sender: TObject);
  private
    FormDsapCode: TFORMDsapCode;{#NAVOPTECH2022-785}
    procedure m_SetContactsDataSet(sender: TObject);
    procedure m_HasSapCodeOnMill(Sender: TObject; var pv_flag: Boolean);
    procedure m_OpenSapCodeForm(Sender: TObject);
    procedure m_CodeChanged(Sender: TObject);
    { Private declarations }
  protected
    procedure m_getData; override;
  public
    { Public declarations }
    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
  end;

var
  FORMMcarrier: TFORMMcarrier;

implementation

{$R *.dfm}

uses
  kneUtils, DRentities, DB, Global;

{ TFORMMcarrier }

class function TFORMMcarrier.m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit;
begin
  // Substituir pelo nome do form
  Result := TFORMMcarrier.Create(Application);
end;

procedure TFORMMcarrier.m_getData;
var
  lv_MasterFrame: TFRAMEBaseEditSOA;
begin
  Screen.Cursor := crHourGlass;
  // optimiza��o de recursos
  lv_MasterFrame := TFRAMEBaseEditSOA(kneUtils.TkneGeneric.fg_GetMasterFrame(Self));

  FRAMEcarrierAvailableVehicle1.MasterSource := lv_MasterFrame.DStable;
  FRAMEcarrierType1.MasterSource := lv_MasterFrame.DStable;       //JAR #10870 2011/10/25

  FRAMElistAddresses1.MasterSource := lv_MasterFrame.DStable;    // Detail <- Master
  FRAMElistContacts1.MasterSource := FRAMElistAddresses1.DStable;   // Detail <- Detail(master)


  // # 3467
  FRAMEcarrierMill1.MasterSource    := lv_MasterFrame.DStable;
  FRAMEcarrierMill1.HasSapCode      := m_HasSapCodeOnMill;
  FRAMEcarrierMill1.OpenSapCodeForm := m_OpenSapCodeForm;

  // parametros standard de servi�os
  lv_MasterFrame.ServiceParams.ShowInactives := True;
//  lv_MasterFrame.ServiceParams.MaxRecords := 0;
//  lv_MasterFrame.ServiceParams.Criteria := '';

  inherited m_getData;

  FRAMElistAddresses1.GetDataSet := m_SetContactsDataSet;   // atribui uma referencia para o dadaset dos Contacts
  PGCaddress.ActivePageIndex := 0;

  //NAVOPTECH2022-875 (cmosilva 22-08-2022)
```

#### **Mcarrier.dfm**

```
inherited FORMMcarrier: TFORMMcarrier
  Left = 402
  Top = 227
  Width = 830
  Height = 525
  Caption = 'Carriers Management'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited PNLtoolbar: TsPanel
    Width = 814
    inherited CLBactions: TsCoolBar
      Width = 814
      Bands = <
        item
          Control = PNbotoes
          ImageIndex = -1
          MinHeight = 41
          Width = 810
        end>
      inherited PNbotoes: TsPanel
        Width = 797
        object PNLsapCode: TsPanel
          Left = 560
          Top = 1
          Width = 81
          Height = 39
          Align = alLeft
          TabOrder = 7
          Visible = False
          SkinData.SkinSection = 'ALPHACOMBOBOX'
          object BTNsapCode: TsBitBtn
            Left = 2
            Top = 2
            Width = 75
            Height = 30
            Action = ACTsapCode
            Caption = 'SAP Code'
            Enabled = False
            TabOrder = 0
            TabStop = False
            NumGlyphs = 2
            SkinData.SkinSection = 'SPEEDBUTTON'
            Images = IMLbuttons
          end
        end
      end
    end
  end
  object PNLcarrier: TsPanel [2]
    Left = 0
    Top = 41
    Width = 814
    Height = 446
    Align = alClient
    TabOrder = 1
    SkinData.SkinSection = 'PANEL'
    object sSplitter1: TsSplitter
      Left = 1
      Top = 214
      Width = 812
      Height = 4
      Cursor = crVSplit
      Align = alTop
      SkinData.SkinSection = 'FORM'
    end
    inline FRAMEcarrier1: TFRAMEcarrier
      Left = 1
      Top = 1
      Width = 812
      Height = 213
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      inherited Label3: TsLabel
        FocusControl = FRAMEcarrier1.FRAMEfindLanguage.DBE
      end
      inherited Label4: TsLabel
        FocusControl = FRAMEcarrier1.FRAMEfindCountry.DBE
      end
      inherited Label5: TsLabel
        FocusControl = FRAMEcarrier1.FRAMEfindCurrency.DBE
      end
      inherited Label6: TsLabel
        FocusControl = FRAMEcarrier1.FRAMEfindPayment.DBE
      end
      inherited PNLfooter: TsPanel
        Top = 179
        Width = 812
      end
      inherited FRAMEfindCountry: TFRAMEFindEditSOA
        inherited PNLdesc: TPanel
          inherited DBEDesc: TsDBEdit
            Width = 240
```
<!-- tabs:end -->

