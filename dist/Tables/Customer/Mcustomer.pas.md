<!-- tabs:start -->

#### **Documentation**

# Documentation for `Mcustomer` Code Unit

---

## 1. Overview:

### Objective:
The `Mcustomer` code unit is designed to manage customer-related data and operations within a software application. It provides a user interface for maintaining customer information, including addresses, contacts, credit information, bank details, and other customer-specific attributes. The form is a comprehensive customer maintenance module that allows users to view, edit, and manage customer data efficiently.

### Technologies Used:
- **Delphi**: The code is written in Delphi, utilizing its VCL (Visual Component Library) for UI components.
- **Third-party Libraries**: Includes components like `TsSplitter`, `TsPanel`, `TsPageControl`, and others from the `s` library, which are likely used for enhanced UI styling and functionality.
- **Database Interaction**: The code interacts with datasets (`DBClient`) to fetch and manipulate customer data.

### Form Type:
This is a **form** with multiple tabs and embedded frames for managing customer details.

#### Form Elements and Their Types:
1. **Tabs** (`TsPageControl` and `TsTabSheet`):
   - Each tab represents a specific category of customer information (e.g., Addresses, Contacts, Credit Info).
2. **Buttons** (`TsBitBtn`, `TsSpeedButton`):
   - Perform actions like saving, canceling, duplicating records, etc.
3. **Splitters** (`TsSplitter`):
   - Used for resizing sections of the form.
4. **Panels** (`TsPanel`):
   - Group and organize UI components.
5. **Frames**:
   - Embedded frames like `FRAMElistAddresses1`, `FRAMEcustAgent1`, etc., handle specific functionalities.

#### Form Actions and Their Effects:
- **Print Current Record**: Prints the currently selected customer record.
- **Duplicate Record**: Creates a duplicate of the selected customer record.
- **New Record**: Initializes a new customer record for creation.
- **Cancel**: Cancels the current operation and reverts changes.

---

## 2. Functionality Description:

### User/Software Actions:
- View and edit customer details across multiple categories.
- Add new customer records.
- Duplicate existing customer records.
- Print customer information.
- Navigate through different customer-related data using tabs.

### Main Components:
1. **Tabs**: Organize customer data into logical sections.
2. **Frames**: Handle specific functionalities like managing addresses, contacts, and credit information.
3. **Buttons**: Trigger actions like saving, canceling, and duplicating records.

### Pseudo-code for Actions and Events:
- `OnClick` event of `BTprintCurrentRecord`: `if button clicked then print current record`.
- `OnClick` event of `BTCancel`: `if button clicked then cancel operation`.
- `OnClick` event of `BTNew`: `if button clicked then initialize new record`.
- `OnExecute` event of `ACTduplicate`: `if action executed then duplicate record`.
- `OnChange` event of `PGCcstDetails`: `if tab changed then load corresponding data`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is initialized with default settings and constraints.
   - Tabs and frames are loaded to display customer-related data.
2. **User Interaction**:
   - Users can navigate through tabs to view/edit specific customer details.
   - Buttons trigger actions like saving, canceling, or duplicating records.
3. **Functions**:
   - `BTprintCurrentRecordClick`: Prints the current record.
   - `BTCancelClick`: Cancels the current operation.
   - `ACTduplicateExecute`: Duplicates the selected record.
   - `BTNewClick`: Creates a new customer record.

### Data Input:
- Users must provide customer details such as name, address, contact information, credit details, etc., depending on the selected tab.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Print**: Requires a record to be selected.
- **Duplicate**: Requires a record to be selected and duplication to be allowed (`FcanDuplicate = True`).
- **Save**: All required fields must be filled and validated.

### Available Filters:
- No explicit filters are defined in the code snippet.

### Error Messages:
- Not explicitly defined in the code snippet.

### Default Field Values:
- Not explicitly defined in the code snippet.

### Field Validation and Conditions:
- Validation logic is implemented in the `m_Validate` method, but specific rules are not detailed in the code snippet.

---

## 5. Main Functions:

1. **`m_getData`**: Fetches data for the form.
2. **`m_Validate`**: Validates the form data before saving.
3. **`m_PutData`**: Saves the data to the database.
4. **`ACTduplicateExecute`**: Handles the duplication of customer records.
5. **`BTprintCurrentRecordClick`**: Prints the current customer record.

---

## 6. API Service Consumption:

No explicit API calls are defined in the provided code snippet.

---

## 7. Conditional Fields (Form Logic):

- The visibility of certain fields or frames may depend on specific conditions, but these are not explicitly defined in the code snippet.

---

## 8. Dependencies:

### External Libraries:
- **`s` Library**: Used for UI components like `TsSplitter`, `TsPanel`, `TsPageControl`, etc.
- **`kne` Components**: Custom components for enhanced functionality.

### Custom Components:
- Frames like `FRAMElistAddresses1`, `FRAMEcustAgent1`, etc., are custom components designed for specific functionalities.

---

## 9. Fields and Validations Listing:

### Fields:
- **Address**: Managed by `FRAMElistAddresses1`.
- **Contacts**: Managed by `FRAMElistContacts1`.
- **Credit Info**: Managed by `FRAMEcustomerCreditInfo1`.
- **Bank Details**: Managed by `FRAMEcustBank1`.

### Mapping:
- Field mappings to database columns are not explicitly defined in the code snippet.

---

## 10. Examples and Diagrams:

### Flowchart:
The workflow involves initializing the form, loading data, user interactions (e.g., navigating tabs, clicking buttons), and saving or canceling operations.

### Sequence Diagram:
1. User opens the form.
2. Data is loaded into tabs and frames.
3. User interacts with the form (e.g., edits data, clicks buttons).
4. Corresponding actions are executed (e.g., save, cancel, duplicate).

### Code Snippets:
```delphi
procedure TFORMMcustomer.BTNewClick(Sender: TObject);
begin
  // Initialize a new customer record
end;
```

### Screenshots:
Not applicable as the DFM file is not fully provided.

---

## 11. Important Comments in the Code:

- `FAddressMasterKeyFields`: Used to define the link with addresses shared by multiple entities.
- `FIsPortalActivated`: Indicates whether the portal is activated (legacy code).

---

## 12. Conclusion:

The `Mcustomer` code unit is a robust customer maintenance module with a well-structured UI and functionality. However, the lack of explicit error handling, field validation rules, and API integration details limits its completeness. Its modular design with frames makes it extensible and maintainable.

---

## 13. Short Summary:

The `Mcustomer` code unit provides a comprehensive interface for managing customer data, including addresses, contacts, and credit information. It supports actions like adding, duplicating, and printing records, with a modular design for extensibility.#### **Mcustomer.pas**

```
unit Mcustomer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBedit, StdCtrls, Buttons, ExtCtrls, ComCtrls, kneFREditSOA,
  FRcustomer, FRcustProdMill, kneFRGridEditSOA, FRcustBia, DBClient,
  kneFRCtrlEditSOA, knePrivileges, ImgList, sSpeedButton,                                                                      
  sBitBtn, ToolWin, acCoolBar, sPanel, kneEnterAsTab, sPageControl,
  ActnList, sSplitter, FRlistContacts, FRlistAddresses, FRcustAgent,
  FRcustomerCreditInfo, FRcustBank, cxStyles, cxControls, cxGrid, FRchanges,
  FRdocumentsInformation, kneTypes, FRcustSalesMan, FRcustomerVAT,
  FRextShipDelCust, FRentityComm, FRcustIBAN, FRcustOrdUnits,
  FRcustBusinessChannel, FRcustPulpPriceType, FRcustCoC,
  FRcustCrosssellBrand;
  
type
  TFORMMcustomer = class(TFORMkneBaseEdit)
    sSplitter1: TsSplitter;
    PGCcstDetails: TsPageControl;
    TSHmill: TsTabSheet;
    FRAMEcustProdMill1: TFRAMEcustProdMill;
    TSHbia: TsTabSheet;
    FRAMEcustBia1: TFRAMEcustBia;
    SHaddresses: TsTabSheet;
    FRAMElistAddresses1: TFRAMElistAddresses;
    SPL2: TsSplitter;
    FRAMElistContacts1: TFRAMElistContacts;                                                                                                                      
    TSHagent: TsTabSheet;
    FRAMEcustAgent1: TFRAMEcustAgent;
    TSHcredInfo: TsTabSheet;
    FRAMEcustomerCreditInfo1: TFRAMEcustomerCreditInfo;
    TSHbanks: TsTabSheet;
    FRAMEcustBank1: TFRAMEcustBank;
    TSHchanges: TsTabSheet;
    FRAMEchanges1: TFRAMEchanges;
    TSHdocuments: TsTabSheet;
    FRAMEdocumentsInformation1: TFRAMEdocumentsInformation;
    PNL1: TsPanel;
    BTNaddress: TsBitBtn;
    ACTduplicate: TAction;
    TSHcustSalesMan: TsTabSheet;
    FRAMEcustSalesMan1: TFRAMEcustSalesMan;
    TSHcustVAT: TsTabSheet;
    FRAMEcustomerVAT1: TFRAMEcustomerVAT;
    TSHextShipDelCust: TsTabSheet;
    FRAMEextShipDelCust1: TFRAMEextShipDelCust;
    TSHentityComm: TsTabSheet;
    FRAMEentityComm1: TFRAMEentityComm;
    TSHcustIBAN: TsTabSheet;
    FRAMEcustIBAN1: TFRAMEcustIBAN;
    FRAMEcustomer1: TFRAMEcustomer;
    TSHcustOrdUnits: TsTabSheet;
    FRAMEcustOrdUnits1: TFRAMEcustOrdUnits;
    TSHbusinessChannel: TsTabSheet;
    FRAMEcustBusinessChannel1: TFRAMEcustBusinessChannel;
    TSHpulpPriceType: TsTabSheet;
    TSHcustCoC: TsTabSheet;
    FRAMEcustCoC1: TFRAMEcustCoC;
    TSHcrosssellingBrands: TsTabSheet;
    FRAMEcustCrosssellBrand1: TFRAMEcustCrosssellBrand;
    FRAMEcustPulpPriceType1: TFRAMEcustPulpPriceType;
    procedure BTprintCurrentRecordClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BTCancelClick(Sender: TObject);
    procedure ACTduplicateExecute(Sender: TObject);
    procedure BTNewClick(Sender: TObject);
    procedure PNbotoesClick(Sender: TObject);
    procedure PGCcstDetailsChange(Sender: TObject);
  private
    { Private declarations }
    FAddressMasterKeyFields: String;   // � necess�ria para definir a liga��o com os address ( uma vez que estes s�o utilizados por varias entidades)
    FcanDuplicate: Boolean;
    FIsPortalActivated: Boolean;  //JAR #5354 23-02-2010 Apagar este c�digo apos int. o duplicate na kneCBEdit

    function GetAddrMasterKeyFields: String;
    procedure m_SetContactsDataSet(sender: TObject);
    procedure m_ReloadedRecord(Sender: TObject);
    procedure SetIsPortalActivated(const Value: Boolean);
    function GetIsPortalActivated: Boolean;

  protected
    procedure m_getData; override;
    function m_Validate: Boolean; override;
    procedure m_PutData; override;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent);

    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
    procedure m_FormEdit(const pv_AccessMode: string; const pv_KeyValues: string = '');  override;

    procedure BIAControl;
    procedure DoProtectControl(Sender: TObject;
      const pv_Args: array of const);
    function SetAllControlsState(const pv_Control: TControl;
      const pv_Enabled: Boolean): Boolean;
    procedure SetProtectFieldsState(Sender: TFRAMEBaseGridEditSOA);
```

#### **Mcustomer.dfm**

```
inherited FORMMcustomer: TFORMMcustomer
  Left = 0
  Top = 103
  Width = 1596
  Height = 828
  Caption = 'Customer Maintenance'
  Constraints.MinHeight = 690
  Constraints.MinWidth = 1050
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object sSplitter1: TsSplitter [1]
    Left = 0
    Top = 507
    Width = 1580
    Height = 4
    Cursor = crVSplit
    Align = alTop
    SkinData.SkinSection = 'FORM'
  end
  inherited PNLtoolbar: TsPanel
    Width = 1580
    TabOrder = 2
    inherited CLBactions: TsCoolBar
      Width = 1580
      Bands = <
        item
          Control = PNbotoes
          ImageIndex = -1
          MinHeight = 41
          Width = 1576
        end>
      inherited PNbotoes: TsPanel
        Width = 1563
        OnClick = PNbotoesClick
        inherited BTNseparator2: TsSpeedButton
          Left = 642
        end
        inherited PNLdelete: TsPanel
          inherited BTNDelete: TsBitBtn
            Glyph.Data = {
              36090000424D3609000000000000360000002800000018000000180000000100
              2000000000000009000000000000000000000000000000000000FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF007B3921006331
              310063313100633131006331310063313100FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF006331310031319C0031319C003131
              CE0031319C003131CE0031319C0031319C003131630031313100FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF009C5A390031319C003131CE003131CE003131CE00315A
              E700315AE700315AE7003131CE003131CE003131CE0031319C0031319C003131
              3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00633163003131CE003131CE00315AE700315AE700315AE700315A
              E700315AE700315AE700315AE700315AE700315AE700315AE7003131CE003131
              9C0031313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF009C6363003131CE00315AE700315AE700315AE700315AE700315AE700315A
              E700315AE700315AE700315AE700315AE700315AE700315AE700315AE7003163
              CE003131CE0031313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
              63003131CE00315AE700315AE700315AE700315AE7003163FF00315AE700315A
              E700315AE700315AE700315AE700315AE700315AE700315AE700315AE700315A
              E7003131CE003131CE0031313100FF00FF00FF00FF00FF00FF00FF00FF003131
              9C00315AE700315AE700315AE700315AE700315AE700315AE700315AE700315A
              E700315AE700315AE700315AE700315AE7003163FF00315AE700315AE7003163
              FF00315AE7003131CE0031319C0063313100FF00FF00FF00FF00B5735A00315A
              E700315AE700315AE700315AE700315AE700315AE700315AE700315AE700315A
              E700315AE7003163FF00315AE700315AE700315AE700315AE700315AE700315A
              E700315AE700315AE7003131CE0031313100FF00FF00FF00FF0063319C00315A
              E700315AE700315AE700315AE700315AE700315AE700315AE700315AE7003163
              FF00315AE700315AE700315AE700315AE700315AE700315AE700315AE700315A
              E700315AE700315AE7003131CE0031319C007B392100FF00FF003163CE00315A
              E700315AE700315AE700315AE700315AE7003163FF00315AE700315AE700315A
              E700315AE700315AE700315AE700315AE700315AE700315AE700315AE700315A
              E700315AE700315AE700315AE7003131CE0063310000FF00FF00315AE700315A
              E7003163FF003163FF00CEEFF700CECEFF00CECEFF00CECEFF00CECEFF00CECE
              FF00CECEFF00CECEFF00CECEFF00CECEFF00CECEFF00CECEFF00CECEFF00CECE
              FF00315AE700315AE700315AE7003131CE0063313100FF00FF00315AE700315A
              E700315AE700315AE700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00315AE700315AE700315AE7003163CE0063313100FF00FF00315AE700315A
              E700319CFF003163FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00315AE700315AE700315AE7003131CE0063313100FF00FF00315AE700315A
              E7006363FF006363CE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00315AE700315AE700315AE700315AE7007B392100FF00FF00315AE700315A
              E700639CFF00639CFF00639CFF00639CFF00639CCE00639CFF006363FF00639C
              FF006363FF003163FF003163CE003163FF003163CE00315AE700315AE700315A
              E700315AE700315AE700315AE7003131CE0094422900FF00FF0063639C00315A
              E700639CFF00639CFF00639CCE00639CFF00639CFF00639CFF00639CCE00639C
              FF00639CCE00639CFF006363FF003163FF003163FF003163FF00315AE7003163
              FF00315AE700315AE700315AE70031319C009C5A3900FF00FF00CE636300315A
              E7006363FF00639CFF00639CFF00639CFF009C9CFF00639CFF00639CFF00639C
              FF00639CFF006363FF00639CCE006363FF00319CCE003163FF00315AE700315A
              E700315AE700315AE700315AE70063313100FF00FF00FF00FF00FF00FF006363
              CE00315AE700639CFF009C9CFF00A5B5F700639CFF009C9CFF00639CFF009C9C
```
<!-- tabs:end -->

