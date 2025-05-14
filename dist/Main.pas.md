<!-- tabs:start -->

#### **Documentation**

# Documentation for `Main` Code Unit

## 1. Overview:

### Objective and Problem Solved:

The `Main` code unit represents the main form (`FORMmain`) of an application, serving as the primary user interface for navigating and managing various business-related entities such as regions, customers, warehouses, carriers, and more. It provides a structured menu and action-based navigation system to access different functionalities. The problem it solves is organizing and centralizing access to multiple business-related operations in a single, user-friendly interface.

### Technologies Used:

- **Delphi (Object Pascal):** The code is written in Delphi, utilizing its VCL (Visual Component Library) for GUI development.
- **Third-party Components:**
  - `TsPanel`, `TsScrollBox`, `TsSplitter`, `TsStatusBar`: Components from the `AlphaControls` library for enhanced UI styling.
  - `TkneActionDisplay`: A custom component for displaying and managing actions.
- **Standard Delphi Components:**
  - `TActionList`, `TAction`: For managing user actions.
  - `TMenuItem`: For creating menu items.
  - `TImage`: For displaying images.

### Form Type:

This is a **form** with the following elements:

- **Form Elements and Types:**
  - **Menu Items:** Representing various business entities and operations.
  - **Action Displays (`TkneActionDisplay`):** Buttons or clickable elements linked to specific actions.
  - **Panels (`TsPanel`):** Used for layout and grouping UI elements.
  - **Scroll Boxes (`TsScrollBox`):** For scrollable content areas.
  - **Splitter (`TsSplitter`):** For resizable sections.
  - **Status Bar (`TsStatusBar`):** For displaying status information.
- **Form Actions and Effects:**
  - Clicking on menu items or action displays triggers specific actions (e.g., opening a list of regions, customers, or warehouses).

---

## 2. Functionality Description:

### User/Software Actions:

- Users can navigate through the menu to access different business-related lists and operations.
- Clicking on action displays or menu items triggers corresponding actions to display or manage data.

### Main Components:

- **Menu Items:** Provide navigation to different sections.
- **Action Displays (`TkneActionDisplay`):** Represent clickable actions for specific functionalities.
- **Panels and Scroll Boxes:** Organize and display content in a structured layout.

### Pseudo-code for Actions and Events:

- **Menu Item Click Event:**  
  `if menu item clicked then execute associated action`
- **Action Display Click Event:**  
  `if action display clicked then execute associated action`

---

## 3. Operational Logic:

### Execution Flow:

1. **Initialization:** The form (`FORMmain`) is initialized, loading all UI components such as panels, menus, and action displays.
2. **User Interaction:** Users interact with the menu or action displays to trigger specific actions.
3. **Action Execution:** Actions are executed, typically opening a new form or displaying a list of data.

### Required User Data:

- No specific data is required to navigate the form. However, actions may require additional input or selections in subsequent forms.

---

## 4. Business Rules:

### Actions and Preconditions:

- **Menu Items and Action Displays:** Actions are triggered when clicked. No specific preconditions are required for these actions.

### Available Filters:

- Filters are not explicitly defined in the provided code.

### Error Messages:

- Error handling is not explicitly defined in the provided code.

### Default Field Values:

- Default values for fields are not explicitly defined in the provided code.

### Field Validation and Conditions:

- Field validations are not explicitly defined in the provided code.

---

## 5. Main Functions:

### Functions:

- **Navigation:** Provides a structured menu and action displays for navigating to different sections of the application.
- **Action Execution:** Executes specific actions when menu items or action displays are clicked.

# Navigation Possibilities

Below is the comprehensive list of all navigation possibilities in the application, as defined in the Pascal code (`TFORMmain`) and the corresponding DFM file. These navigation options are available through **menus**, **action displays**, and **shortcut actions**.

---

## 1. Tables Menu

The **Tables Menu** provides options for managing various entities. Each menu item is associated with a specific action that opens a corresponding form.

The **Action Displays** are visual clickable buttons organized within the UI (e.g., `PNLnavigation`). These buttons correspond to the actions and provide quick access to functionalities.

### Groups of Action Displays

#### Table-related Action Displays (in `PNLtables`):

- **Regions** [`TFORMLregion`](/Tables\Region\Lregion.pas)
- **Customer Market** [`TFORMLcustMarket`](/Tables\CustomerMarket\LcustMarket.pas)
- **Consignee Market** [`TFORMLconsMarket`](/LconsMarket.pas)
- **Country Calendar** [`TFORMLcountryCal`](/Tables\CountryCalendar\LcountryCal.pas)
- **States** [`TFORMLstates`](/Tables\State\Lstates.pas)
- **Payment Codes** [`TFORMLpaymentCodes`](/Tables\PaymentCode\LpaymentCodes.pas)
- **Agents** [`TFORMLagents`](/Tables\Agent\Lagents.pas)
- **Warehouse and Platforms** [`TFORMLwarehouse`](/Tables\WarehouseAndPlatform\Lwarehouse.pas)
- **Carriers** [`TFORMLcarrier`](/Tables\Carrier\Lcarrier.pas)
- **Customers** [`TFORMLcustomer`](/Tables\Customer\Lcustomer.pas)
- **Consignees** [`TFORMLconsignee`](/Tables\Consignee\Lconsignee.pas)
- **Business Entities** [`TFORMLebEntityLink`](/Tables\eBussinessEntities\LebEntityLink.pas)
- **Customer Group** [`TFORMLcustomerGroup`](/Tables\CustomerGroup\LcustomerGroup.pas)
- **Bank Accounts** [`TFORMLbankAccounts`](/Tables\BankAccounts\LbankAccounts.pas)
- **Docs Check List** [`TFORMLdocsCheckList`](/Tables\DocsCheckList\LdocsCheckList.pas)
- **Docs Check Defaults** [`TFORMLdocsCheckListDefaults`](/Tables\DocsCheckDefaults\LdocsCheckListDefaults.pas)
- **Docs Check List Rules** [`TFORMLdocsCheckListRules`](/Tables\DocsCheckListRules\LdocsCheckListRules.pas)
- **IKAM** [`TFORMLikam`](/Tables\IKAM\Likam.pas)
- **Sales Region** [`TFORMLsalesRegion`](/Tables\SalesRegion\LsalesRegion.pas)
- **Customer Lists** [`TFORMLcustLists`](/Tables\CustomerLists\LcustLists.pas)
- **Customer Sales Assistants** [`TFORMLcustSalesAssist`](/Offices\CustomerSalesAssistants\LcustSalesAssist.pas)
- **Email Templates** `TFORMLemailTpl` - Not defined in the provided code.

#### Office-related Action Displays (in `PNLoffices`):

- **Sales Manager** [`TFORMLsalesMan`](/Offices\SalesManager\LsalesMan.pas)
- **Sales Assistants** [`TFORMLsalesAssist`](/Offices\SalesAssistants\LsalesAssist.pas)
- **Sales Direction** [`TFORMLsalesDir`](/Offices\SalesDirection\LsalesDir.pas)
- **Sales Offices** [`TFORMLsalesOffices`](/Offices\SalesOffices\LsalesOffices.pas)
- **Back Offices** [`TFORMLbackOffice`](/Offices\BackOffices\LbackOffice.pas)
- **Back Office Assistants** [`TFORMLBackAssist`](/Offices\BackOfficeAssistants\LBackAssist.pas)

#### Business Unit Management/Selection:

- **Business Unit Selection** (`m_ChangeBusUnit`)

---

## 5. Dynamic Content Panels

The `PNLdesktop` panel dynamically displays forms or content based on the action executed from the menus or action displays. For example:

- Selecting "Regions" opens the region management form.
- Selecting "Sales Assistants" opens the sales assistant management form.

---

## 6. Startup Actions

Upon application startup, the `FormCreate` event is triggered, which ensures:

- **Initialization of Default Settings**
- **Business Unit Selection** (if required)

If the user does not select a business unit, the application offers a dialog to select one or exits gracefully.

---

## 6. API Service Consumption:

- No API service consumption is explicitly defined in the provided code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the provided code.

---

## 8. Dependencies:

### External Libraries:

- **AlphaControls:** Used for enhanced UI components (`TsPanel`, `TsScrollBox`, `TsSplitter`, `TsStatusBar`).

### Custom Components:

- **TkneActionDisplay:** A custom component for displaying and managing actions.

---

## 9. Fields and Validations Listing:

- **Fields:** Not explicitly defined in the provided code.
- **Validations:** Not explicitly defined in the provided code.
- **Mapping:** Not explicitly defined in the provided code.

---

## 10. Examples and Diagrams:

### Flowchart:

The workflow involves:

1. Form initialization.
2. User interaction with menu items or action displays.
3. Execution of corresponding actions.

### Sequence Diagram:

1. User clicks a menu item or action display.
2. The associated action is executed.

### Code Snippets:

```pascal
// Example of linking an action to a menu item
ACTlistRegion.OnExecute := procedure begin
  ShowMessage('Region list action executed');
end;
```

### Screenshots:

The provided DFM file represents a form with a menu and action displays. Below is an HTML representation of the form layout:

```html
<div style="width: 822px; height: 692px; border: 1px solid black;">
  <div style="height: 30px; background-color: #f0f0f0; text-align: center;">
    FORMmain - Ecran Principal
  </div>
  <div style="display: flex; height: 565px;">
    <div style="width: 710px; overflow-y: auto; border-right: 1px solid black;">
      <div style="padding: 10px;">
        <button style="width: 160px; height: 40px;">Region</button>
        <button style="width: 160px; height: 40px;">Customer Market</button>
        <button style="width: 160px; height: 40px;">Consignee Market</button>
      </div>
    </div>
    <div style="flex-grow: 1; background-color: #e0e0e0;">Content Area</div>
  </div>
  <div style="height: 20px; background-color: #d0d0d0;">Status Bar</div>
</div>
```

---

## 11. Important Comments in the Code:

- The `TkneActionDisplay` components are used extensively for linking actions to UI elements.
- The `TActionList` manages all actions in a centralized manner.

---

## 12. Conclusion:

The `Main` code unit provides a well-structured main form for navigating and managing various business-related entities. Its strengths lie in its modular design and use of actions for navigation. However, the lack of explicit error handling, field validations, and API integration limits its functionality.

---

## 13. Short Summary:

The `Main` code unit defines a primary form for navigating and managing business entities using a menu and action displays. It provides a modular and user-friendly interface but lacks explicit error handling and validations.

#### **Main.pas**

```
unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBmain, Menus, StdActns, ActnList, ImgList, ComCtrls,
  AppEvnts, knePrivileges, sScrollBox, StdCtrls, sLabel, ExtCtrls, sPanel,
  sStatusBar, sSplitter, kneFRactionDisplay;

type
  TFORMmain = class(TFORMkneBaseMain)
    ACLmain: TActionList;
    ACTrep: TAction;
    abelas1: TMenuItem;
    Regions1: TMenuItem;
    CustomerMarket1: TMenuItem;
    ConsigneeMarket1: TMenuItem;
    CountryCalendar1: TMenuItem;
    States1: TMenuItem;
    PaymentCodes1: TMenuItem;
    Carrier1: TMenuItem;
    WarehouseandPlatforms1: TMenuItem;
    Carriers1: TMenuItem;
    Customers1: TMenuItem;
    Consignee1: TMenuItem;
    ACTlistRegion: TAction;
    ACTlistCustMarket: TAction;
    ACTlistConsMarket: TAction;
    ACTlistCountryCal: TAction;
    ACTlistState: TAction;
    ACTlistPaymentCode: TAction;
    ACTlistAgents: TAction;
    ACTlistWhsePlat: TAction;
    ACTlistCarrier: TAction;
    ACTlistCustomer: TAction;
    ACTlistConsignee: TAction;
    ACTDregion: TkneActionDisplay;
    kneActionDisplay1: TkneActionDisplay;
    kneActionDisplay2: TkneActionDisplay;
    kneActionDisplay3: TkneActionDisplay;
    kneActionDisplay4: TkneActionDisplay;
    kneActionDisplay5: TkneActionDisplay;
    kneActionDisplay6: TkneActionDisplay;
    kneActionDisplay7: TkneActionDisplay;
    kneActionDisplay8: TkneActionDisplay;
    kneActionDisplay9: TkneActionDisplay;
    kneActionDisplay10: TkneActionDisplay;
    ACTlistSalesMan: TAction;
    ACTlistEntityLink: TAction;
    BusinessEntities1: TMenuItem;
    kneActionDisplay11: TkneActionDisplay;
    ACTlistSalesAssist: TAction;
    Offices1: TMenuItem;
    SalesAssistants2: TMenuItem;
    SalesManager1: TMenuItem;
    sPanel2: TsPanel;
    PNLofficesHeader: TsPanel;
    sLabel1: TsLabel;
    Image1: TImage;
    PNLoffices: TsScrollBox;
    knctndsply1: TkneActionDisplay;
    kneActionDisplay12: TkneActionDisplay;
    ACTlistSalesDir: TAction;
    kneActionDisplay13: TkneActionDisplay;
    SalesDirection1: TMenuItem;
    kneActionDisplay14: TkneActionDisplay;
    ACTSalesOffices: TAction;
    SalesOffices1: TMenuItem;
    kneActionDisplay15: TkneActionDisplay;
    ACTcustomerGroup: TAction;
    CustomerGroup1: TMenuItem;
    ACTlistBankAccounts: TAction;
    knctndsply2: TkneActionDisplay;
    MNUlistBankAccounts: TMenuItem;
    ACTlistDocsCheck: TAction;
    FRAME1: TkneActionDisplay;
    DocsCheckList1: TMenuItem;
    ACTlistDocsCheckDefaults: TAction;
    DocsCheckDefaults1: TMenuItem;
    FRAME2: TkneActionDisplay;
    ACTlistIKAM: TAction;
    ACTlistIKAM1: TMenuItem;
    FRAME3: TkneActionDisplay;
    kneActionDisplay16: TkneActionDisplay;
    ACTlistBackOffice: TAction;
    BackOffice1: TMenuItem;
    kneActionDisplay17: TkneActionDisplay;
    ACTlistBackAssist: TAction;
    BackOfficeAssistant1: TMenuItem;
    kneActionDisplay18: TkneActionDisplay;
    ACTlistCustSalesAssist: TAction;
    mnilistCustSalesAssist: TMenuItem;
    ACTlistEmailTpl: TAction;
    MNIemailTpl: TMenuItem;
    ACTlistCustomerLists: TAction;
    MNIlistCustomer: TMenuItem;
    ACTDSPL1: TkneActionDisplay;
    ACTlistDocsCheckListRules: TAction;
    MNIlistDocsCheckListRules: TMenuItem;
    ACTDSPL2: TkneActionDisplay;
    ACTListSalesRegion: TAction;
    SalesRegion1: TMenuItem;
    kneActionDisplay19: TkneActionDisplay;
    ACTchangeBusinessUnit: TAction;
    BusinessUnit1: TMenuItem;
    ChangeBusinessUnit1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure ACTlistRegionExecute(Sender: TObject);
    procedure ACTlistCustMarketExecute(Sender: TObject);
    procedure ACTlistConsMarketExecute(Sender: TObject);
    procedure ACTlistCountryCalExecute(Sender: TObject);
    procedure ACTlistStateExecute(Sender: TObject);
    procedure ACTlistPaymentCodeExecute(Sender: TObject);
    procedure ACTlistAgentsExecute(Sender: TObject);
    procedure ACTlistWhsePlatExecute(Sender: TObject);
    procedure ACTlistCarrierExecute(Sender: TObject);
    procedure ACTlistCustomerExecute(Sender: TObject);
    procedure ACTlistConsigneeExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ACTlistSalesManExecute(Sender: TObject);
    procedure ACTlistEntityLinkExecute(Sender: TObject);
    procedure ACTlistSalesAssistExecute(Sender: TObject);
    procedure ACTlistSalesDirExecute(Sender: TObject);
    procedure ACTSalesOfficesExecute(Sender: TObject);
    procedure ACTcustomerGroupExecute(Sender: TObject);
    procedure ACTlistBankAccountsExecute(Sender: TObject);
    procedure ACTlistDocsCheckExecute(Sender: TObject);
    procedure ACTlistDocsCheckDefaultsExecute(Sender: TObject);
    procedure ACTlistIKAMExecute(Sender: TObject);
    procedure ACTlistBackOfficeExecute(Sender: TObject);
    procedure ACTlistBackAssistExecute(Sender: TObject);
    procedure ACTlistCustSalesAssistExecute(Sender: TObject);
    procedure ACTlistEmailTplExecute(Sender: TObject);
    procedure ACTlistCustomerListsExecute(Sender: TObject);
    procedure ACTlistDocsCheckListRulesExecute(Sender: TObject);
    procedure ACTListSalesRegionExecute(Sender: TObject);
    procedure ACTchangeBusinessUnitExecute(Sender: TObject);
  private
    procedure m_GlobalInit;
    procedure m_GetUserBusUnitsAndDefault;
    function m_ChangeBusUnit: Boolean;
    { Private declarations }
  protected
    function m_InitApplication: Boolean; override;
  public
    { Public declarations }
  end;

var
  FORMmain: TFORMmain;

implementation

uses
  Global {#22272},
  kneUtils, kneTypes,
  //---
  Lagents, Lcarrier, Lconsignee, LconsMarket,
  LcountryCal, LcustMarket, Lcustomer, LpaymentCodes, Lregion, Lstates,
  Lwarehouse, LsalesMan, LebEntityLink, LsalesAssist, LsalesDir, LsalesOffices,
  LcustomerGroup, LbankAccounts, LdocsCheckList, LdocsCheckListDefaults,
  Likam{15133}, LbackOffice{#19942}, LBackAssist{#19945},
  LcustSalesAssist{#21663}
  , BusinessUnitServiceUtils
  , LcustLists {#22903}
  , LdocsCheckListRules {#23516}
  , LsalesRegion {#23669}, kneFindDialog {#NAVOPTECH2022-1386}
  , kneDialogFactory {#NAVOPTECH2022-1386};

{$R *.dfm}

{ TFORMmain }

function TFORMmain.m_InitApplication: Boolean;
begin
  Result := inherited m_InitApplication;

  if Result then
  begin
    m_GlobalInit;
  end;
end;

procedure TFORMmain.m_GlobalInit;
begin
//  m_GetUserBusUnitsAndDefault;

end;


procedure TFORMmain.m_GetUserBusUnitsAndDefault;
var
  lv_Service: TBusinessUnitServiceUtils;
begin

  lv_Service := nil;

  lv_Service := TBusinessUnitServiceUtils.Create(nil);
  try
    try

      gv_BusUnitList := lv_Service.GetBusinessUnit(True {pv_WithCheckAccess}) ;

      //(se as variaveis nao existirem nao da' erro)
      gv_DefaultBusUnit := lv_Service.GetUserBusinessUnitDef; // Default Business Unit


    except
      on E: Exception do
      begin
        TkneLog.OutputSilentException('Error in TFORMmain.m_GetUserBusUnitsAndDefault' + #13#10 + E.Message);
      end;
    end; // try - except

  finally
    if Assigned(lv_Service) then FreeAndNil(lv_Service);
  end;

end;


procedure TFORMmain.FormCreate(Sender: TObject);
begin
  // Atribui��o do tipo de liga��o
  FDataConnectionType := kneDctSOA;

  inherited;

  if not Application.Terminated then
  begin
    if not m_ChangeBusUnit then
      Application.Terminate;

//    m_GetDefaults; // [28-06-2018, #23410]
//    GetDefaultBrowser;
  end;
end;

procedure TFORMmain.ACTlistRegionExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLregion.CreateListForm(Self), Sender);
end;

procedure TFORMmain.ACTlistCustMarketExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLcustMarket.CreateListForm(Self), Sender);
end;

procedure TFORMmain.ACTlistConsMarketExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLconsMarket.CreateListForm(Self), Sender);
end;

procedure TFORMmain.ACTlistCountryCalExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLcountryCal.CreateListForm(Self), Sender);
end;

procedure TFORMmain.ACTlistStateExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLstates.CreateListForm(Self), Sender);
end;

procedure TFORMmain.ACTlistPaymentCodeExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLpaymentCodes.CreateListForm(Self), Sender);
end;

procedure TFORMmain.ACTlistAgentsExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLagents.CreateListForm(Self), Sender);
end;

procedure TFORMmain.ACTlistWhsePlatExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLwarehouse.CreateListForm(Self), Sender);
end;

procedure TFORMmain.ACTlistCarrierExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLcarrier.CreateListForm(Self), Sender);
end;

procedure TFORMmain.ACTlistCustomerExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLcustomer.CreateListForm(Self), Sender);
end;

procedure TFORMmain.ACTlistConsigneeExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLconsignee.CreateListForm(Self), Sender);
end;

procedure TFORMmain.FormActivate(Sender: TObject);
begin
  inherited;
  // lan�ar ac��o
  m_ExecuteAction(TkneGeneric.fg_GetCmdLineSwitch('EXECUTE_ACTION'), ACLmain);
end;

procedure TFORMmain.ACTlistSalesManExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLsalesMan.CreateListForm(Self), Sender);
end;

procedure TFORMmain.ACTlistEntityLinkExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLebEntityLink.CreateListForm(Self), Sender);
end;

procedure TFORMmain.ACTlistSalesAssistExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLsalesAssist.CreateListForm(Self), Sender);
end;

procedure TFORMmain.ACTlistSalesDirExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLsalesDir.CreateListForm(Self), Sender);
end;

procedure TFORMmain.ACTSalesOfficesExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLsalesOffices.CreateListForm(Self), Sender);
end;

procedure TFORMmain.ACTcustomerGroupExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLcustomerGroup.CreateListForm(Self), Sender);
end;

procedure TFORMmain.ACTlistBankAccountsExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLbankAccounts.CreateListForm(Self), Sender);
end;


procedure TFORMmain.ACTlistDocsCheckExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLdocsCheckList.CreateListForm(Self), Sender);
end;

procedure TFORMmain.ACTlistDocsCheckDefaultsExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLdocsCheckListDefaults.CreateListForm(Self), Sender);
end;

procedure TFORMmain.ACTlistIKAMExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLikam.CreateListForm(Self), Sender);      //JAR #15133  08-02-2013

end;

procedure TFORMmain.ACTlistBackOfficeExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLbackOffice.CreateListForm(Self), Sender); // [2014/07/07, #19942]

end;

procedure TFORMmain.ACTlistBackAssistExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLBackAssist.CreateListForm(Self), Sender); // [2014/07/07, #19945]
end;

procedure TFORMmain.ACTlistCustSalesAssistExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLcustSalesAssist.CreateListForm(Self), Sender); // [07-04-2015, #21663]
end;

procedure TFORMmain.ACTlistEmailTplExecute(Sender: TObject);
begin
  inherited;
//  ShowListWindow(TFORMLemailTpl.CreateListForm(Self), Sender);
end;

procedure TFORMmain.ACTlistCustomerListsExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLcustLists.CreateListForm(Self), Sender);  // [#22930]
end;

procedure TFORMmain.ACTlistDocsCheckListRulesExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLdocsCheckListRules.CreateListForm(Self), Sender); // [#23516]
end;

procedure TFORMmain.ACTListSalesRegionExecute(Sender: TObject);
begin
  inherited;
  ShowListWindow(TFORMLsalesRegion.CreateListForm(Self), Sender); // [#23669]
end;

function TFORMmain.m_ChangeBusUnit: Boolean;
var
  lv_ListForm : TFORMkneFindDialog;
  lv_Svc : TBusinessUnitServiceUtils;
  lv_Result: TModalResult;
  lv_Enabled : Boolean;
begin

  Result := False;

  lv_Enabled := False;

  // se o valor da vari�vel for alterado, os forms abertos ser�o encerrados
  // ( inclu�dos na lista de forms abertos - apresentados na op��o de menu 'Window' )
  if ACTCloseAll.Enabled then
      ACTCloseAll.Execute;
  // se existirem forms ainda em edi��o, n�o deve permitir alterar a BusUnit
  if ACTCloseAll.Enabled then
   Exit;


  lv_ListForm := nil;

  lv_Svc := TBusinessUnitServiceUtils.Create(nil);

  try
    try

      lv_ListForm := TkneDialogFactory.GetFindDialog(self);

      if Assigned(lv_ListForm) then
      begin

        lv_ListForm.Caption := 'Business Unit Selection';

        lv_ListForm.ProviderService := lv_Svc;

        lv_ListForm.Options.DataSelection.FieldNameForCode:= 'businessUnit';
        lv_ListForm.Options.DataSelection.FieldNamesForDesc.clear;
        lv_ListForm.Options.DataSelection.DefineFieldsForDesc('businessDesc');

        lv_ListForm.Options.DataSelection.ExitOneRecord := True; // sair se apenas tiver um registo (seleccionando-o)

        lv_ListForm.Find;

        lv_Result := lv_ListForm.ModalResult;

        if  ( lv_Result = mrOK ) then
        begin

          gv_DefaultBusUnit := lv_listForm.SelectedCodeValue;
          gv_BusUnitList    := gv_DefaultBusUnit;

          Application.Title := 'Entity Management [ ' + gv_DefaultBusUnit +' ]';
          UpdateFormTitle('');

          Result := True;
        end
        else if (lv_Result = mrCancel) AND (gv_DefaultBusUnit = '') then
          begin
            if MessageDlg('No Business Unit was selected. Because it''s mandatory, the application will be terminated, do you want to proceed?',mtWarning, [mbYes,mbNo],0) = mrYes then
            begin
              Result := false;
            end else
            begin
              Result := True;

              if not m_ChangeBusUnit then
                Application.Terminate;

            end; // else message

          end // else modalResult
        else // Caso em que cancela mas se pretende manter a whse j� seleccionada
          Result := true;

      end;  // if assigned

    except

      on e: Exception do
      begin
        Result := False;
        ShowMessage(e.message + #13#10
          +'Unable to connect to the service. The application will be terminated. Contact your system administrator.');
      end;


    end;
  finally
    if Assigned(lv_Svc) then FreeAndNil(lv_Svc);
  end;
end;

//NAVOPTECH2022-1386 (cmosilva 10-11-2022)
procedure TFORMmain.ACTchangeBusinessUnitExecute(Sender: TObject);
begin
  inherited;

  m_ChangeBusUnit;

end;

end.

```

#### **Main.dfm**

```
inherited FORMmain: TFORMmain
  Left = 673
  Top = 239
  Width = 822
  Height = 692
  Caption = 'FORMmain - Ecran Principal'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited SPTnavigation: TsSplitter
    Left = 710
    Height = 565
  end
  inherited SBstatus: TsStatusBar
    Top = 615
    Width = 806
  end
  inherited PNLspacer: TsPanel
    Width = 806
    inherited sPanel1: TsPanel
      Top = 15
      Width = 796
      Height = 30
    end
  end
  inherited PNLdesktop: TsPanel
    Left = 720
    Width = 86
    Height = 565
  end
  inherited PNLnavigation: TsScrollBox
    Width = 710
    Height = 565
    inherited PNLtables: TsPanel
      Width = 706
      Height = 265
      inherited PNLtableHeader: TsPanel
        Width = 700
      end
      inherited PNLtableDescs: TsScrollBox
        Width = 700
        Height = 227
        object ACTDregion: TkneActionDisplay
          Left = 0
          Top = 0
          Width = 160
          Height = 40
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
          Action = ACTlistRegion
        end
        object kneActionDisplay1: TkneActionDisplay
          Left = 0
          Top = 40
          Width = 160
          Height = 40
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 1
          Action = ACTlistCustMarket
        end
        object kneActionDisplay2: TkneActionDisplay
          Left = 0
          Top = 80
          Width = 160
          Height = 40
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 2
          Action = ACTlistConsMarket
        end
        object kneActionDisplay3: TkneActionDisplay
          Left = 0
          Top = 120
          Width = 160
          Height = 40
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 3
          Action = ACTlistCountryCal
        end
        object kneActionDisplay4: TkneActionDisplay
          Left = 165
          Top = 0
          Width = 185
          Height = 40
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 4
          Action = ACTlistState
        end
        object kneActionDisplay5: TkneActionDisplay
          Left = 165
          Top = 40
          Width = 185
          Height = 40
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 5
          Action = ACTlistPaymentCode
        end
        object kneActionDisplay6: TkneActionDisplay
          Left = 165
          Top = 80
          Width = 185
          Height = 40
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 6
          Action = ACTlistAgents
        end
        object kneActionDisplay7: TkneActionDisplay
          Left = 165
          Top = 120
          Width = 185
          Height = 40
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 7
          Action = ACTlistWhsePlat
        end
        object kneActionDisplay8: TkneActionDisplay
          Left = 354
          Top = 0
          Width = 150
          Height = 40
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 8
          Action = ACTlistCarrier
        end
        object kneActionDisplay9: TkneActionDisplay
          Left = 354
          Top = 40
          Width = 150
          Height = 40
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 9
          Action = ACTlistCustomer
        end
        object kneActionDisplay10: TkneActionDisplay
          Left = 354
          Top = 80
          Width = 150
          Height = 40
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 10
          Action = ACTlistConsignee
        end
        object kneActionDisplay11: TkneActionDisplay
          Left = 354
          Top = 120
          Width = 150
          Height = 40
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 11
          Action = ACTlistEntityLink
        end
        object kneActionDisplay15: TkneActionDisplay
          Left = 0
          Top = 160
          Width = 160
          Height = 44
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 12
          Action = ACTcustomerGroup
        end
        object knctndsply2: TkneActionDisplay
          Left = 164
          Top = 160
          Width = 185
          Height = 44
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 13
          Action = ACTlistBankAccounts
        end
        object FRAME1: TkneActionDisplay
          Left = 354
          Top = 160
          Width = 150
          Height = 44
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 14
          Action = ACTlistDocsCheck
        end
        object FRAME2: TkneActionDisplay
          Left = 508
          Top = 0
          Width = 160
          Height = 40
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 15
          Action = ACTlistDocsCheckDefaults
        end
        object FRAME3: TkneActionDisplay
          Left = 508
          Top = 40
          Width = 160
          Height = 40
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 16
          Action = ACTlistIKAM
        end
        object ACTDSPL1: TkneActionDisplay
          Left = 508
          Top = 80
          Width = 160
          Height = 40
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 17
          Action = ACTlistCustomerLists
        end
        object ACTDSPL2: TkneActionDisplay
          Left = 508
          Top = 120
          Width = 160
          Height = 41
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 18
          Action = ACTlistDocsCheckListRules
        end
        object kneActionDisplay19: TkneActionDisplay
          Left = 509
          Top = 161
          Width = 159
          Height = 44
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 19
          Action = ACTListSalesRegion
        end
      end
    end
    inherited PNLfunctions: TsPanel
      Top = 417
      Width = 706
      Visible = False
      inherited PNLfunctionHeader: TsPanel
        Width = 700
      end
      inherited PNLfunctionDescs: TsScrollBox
        Width = 700
      end
    end
    inherited PNLreports: TsPanel
      Top = 545
      Width = 706
      Visible = False
      inherited PNLreportHeader: TsPanel
        Width = 700
      end
      inherited PNLreportDescs: TsScrollBox
        Width = 700
      end
    end
    object sPanel2: TsPanel
      Left = 0
      Top = 265
      Width = 706
      Height = 152
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 3
      SkinData.SkinSection = 'ALPHACOMBOBOX'
      object PNLofficesHeader: TsPanel
        Left = 3
        Top = 3
        Width = 700
        Height = 32
        Hint = 'Click to show/hide details'
        Align = alTop
        BevelOuter = bvNone
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = PNLfunctionHeaderClick
        SkinData.SkinSection = 'SPEEDBUTTON'
        object sLabel1: TsLabel
          Left = 40
          Top = 8
          Width = 44
          Height = 16
          Caption = 'Offices'
          ParentFont = False
          OnClick = PNLfunctionHeaderClick
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
        end
        object Image1: TImage
          Left = 8
          Top = 4
          Width = 24
          Height = 24
          Picture.Data = {
            055449636F6E0000010001001818000001000800C80600001600000028000000
            1800000030000000010008000000000000000000000000000000000000000000
            00000000FFFFFF00CCFFFF0099FFFF0066FFFF0033FFFF0000FFFF00FFCCFF00
            CCCCFF0099CCFF0066CCFF0033CCFF0000CCFF00FF99FF00CC99FF009999FF00
            6699FF003399FF000099FF00FF66FF00CC66FF009966FF006666FF003366FF00
            0066FF00FF33FF00CC33FF009933FF006633FF003333FF000033FF00FF00FF00
            CC00FF009900FF006600FF003300FF000000FF00FFFFCC00CCFFCC0099FFCC00
            66FFCC0033FFCC0000FFCC00FFCCCC00CCCCCC0099CCCC0066CCCC0033CCCC00
            00CCCC00FF99CC00CC99CC009999CC006699CC003399CC000099CC00FF66CC00
            CC66CC009966CC006666CC003366CC000066CC00FF33CC00CC33CC009933CC00
            6633CC003333CC000033CC00FF00CC00CC00CC009900CC006600CC003300CC00
            0000CC00FFFF9900CCFF990099FF990066FF990033FF990000FF9900FFCC9900
            CCCC990099CC990066CC990033CC990000CC9900FF999900CC99990099999900
            669999003399990000999900FF669900CC669900996699006666990033669900
            00669900FF339900CC33990099339900663399003333990000339900FF009900
            CC00990099009900660099003300990000009900FFFF6600CCFF660099FF6600
            66FF660033FF660000FF6600FFCC6600CCCC660099CC660066CC660033CC6600
            00CC6600FF996600CC99660099996600669966003399660000996600FF666600
            CC66660099666600666666003366660000666600FF336600CC33660099336600
            663366003333660000336600FF006600CC006600990066006600660033006600
            00006600FFFF3300CCFF330099FF330066FF330033FF330000FF3300FFCC3300
            CCCC330099CC330066CC330033CC330000CC3300FF993300CC99330099993300
            669933003399330000993300FF663300CC663300996633006666330033663300
            00663300FF333300CC33330099333300663333003333330000333300FF003300
            CC00330099003300660033003300330000003300FFFF0000CCFF000099FF0000
            66FF000033FF000000000000FFCC0000CCCC000099CC000066CC000033CC0000
            00CC0000FF990000CC99000099990000669900003399000000990000FF660000
            CC66000099660000666600003366000000660000FF330000CC33000099330000
            663300003333000000330000FF000000CC000000990000006600000033000000
            00000000934528007F3B23003D1C1100B0735E009E593C00E8D5C400D5B49300
            AA600F00B77E3900B4711800C2925300855C2000BA750300C9AB7B00F3E7D300
            CA890900E7D0A500E2B04500EAC98300DB9F0800E2AB240079701800717C1400
            678A0D0052AD040057B11B0060B83F0083C86A00AEDB9F00D5ECD10066BD5A00
            BDE2BA0070C57C00F0F7F20095E5F700CAEDF700B6D5E200A4B6F1003458E600
            FFFFFF00B9B9B9B9B9B9B9B9B9B9B9B9B9B9B9B9B9B9B9B9B9B9B9B9B9B9B9B9
            AAD9D9ABABABABABABD9D9B9B9B9B9B9B9B9B9B9B9B9AAD9D8DFDFE1DFDFE1DF
            DFD8D9CFABB9B9B9B9B9B9B9B9D8DFE1E7A3E7E7C7A3C7C7C7E1DFDFD8CFABB9
            B9B9B9B9DFE19DA3A3A3A3A3E7C7E7E7C7C7C7C7DFDFD9D9B9B9B9B9E1A39D9D
            9D9D9DA39DA3A3A3C7C7C7C7C7C7E3D9B9B9B9B9E1E8E8E8E8E8E8E8E8E8EAEA
            DEEAEAEAEAEADBDAABABB9B9E0DC86AB86AB86AB86ABAA87DC87AB87ABAB87AB
            ABD9ABB9E1DBFEFEFEFEFEFEFEFE16FEFE404040404040404087ABB9E1DB160F
            160F160F160F160F1640416447656B6B6B8ECFB9E0DB0E0F0E0F0E0F0E0F0E0F
            0E6B6B6B6B6B6B6B6B87ABB9E0800707FB07FB0707070707FB6B6B6B6B6B6B6B
            6B8ED8B9E079DBDBDCDBDCDBDBDBDCDBDCDBDC80DBDBDCD9AAD8B9B9E0E6E6E6
            E62ADDE8E8E8E84EDEEAEA78E5E9DCD9B9B9B9B9E0E6E6E62ADDE84EDEEAEAE5
            7879799D9CA3E3D9B9B9B9B9E2F9F9F9F9E6E6DD4EDEDE78E578E9E9A39DD8D9
            B9B9B9B9E2F900F9F9F9E62ADD4EEADEEA7979799D9DD8D9B9B9B9B9E2F9F900
            F9F9F9DDE8EADEEAEA787879789DD8D9B9B9B9B9E200F9E62AE8EADE78E5E99D
            A39DA39D9D9DD8D9B9B9B9B9E2DDE8DDE8E8E8EADE7879E9A3E7C7C7C7A3DFD9
            B9B9B9B9E0DD2AE6E6DD2AE8EAE578799DA3C7C7C7C7E3D8B9B9B9B9DFDEF900
            F900F9E62A4E79789D9DA3C7C7E1D8A9B9B9B9B9B9DFE0E5DDE600F9F9E8E879
            E9A3E1DFDFDFA3B9B9B9B9B9B9B9B9A3DFDFDFDFDFDFDFDFDFDFE1A3A3B9B9B9
            B9B9B9B9FFFFFF00F001FF00C0007F0080001F0000000F0000000F0000000300
            00000100000001000000010000000100000001000000030000000F0000000F00
            00000F0000000F0000000F0000000F0000000F0000000F0000000F0080001F00
            E0007F00}
        end
      end
      object PNLoffices: TsScrollBox
        Left = 3
        Top = 35
        Width = 700
        Height = 114
        Align = alClient
        TabOrder = 1
        SkinData.SkinSection = 'PANEL_LOW'
        object knctndsply1: TkneActionDisplay
          Left = 0
          Top = 44
          Width = 141
          Height = 44
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
          Action = ACTlistSalesMan
        end
        object kneActionDisplay12: TkneActionDisplay
          Left = 142
          Top = 44
          Width = 150
          Height = 44
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 1
          Action = ACTlistSalesAssist
        end
        object kneActionDisplay13: TkneActionDisplay
          Left = 0
          Top = 0
          Width = 141
          Height = 44
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 2
          Action = ACTlistSalesDir
        end
        object kneActionDisplay14: TkneActionDisplay
          Left = 142
          Top = 0
          Width = 147
          Height = 44
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 3
          Action = ACTSalesOffices
        end
        object kneActionDisplay17: TkneActionDisplay
          Left = 293
          Top = 44
          Width = 177
          Height = 44
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 4
          Action = ACTlistBackAssist
        end
        object kneActionDisplay18: TkneActionDisplay
          Left = 471
          Top = 1
          Width = 195
          Height = 44
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 5
          Action = ACTlistCustSalesAssist
        end
        object kneActionDisplay16: TkneActionDisplay
          Left = 295
          Top = 0
          Width = 178
          Height = 44
          AutoScroll = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 6
          Action = ACTlistBackOffice
        end
      end
    end
  end
  inherited MNmain: TMainMenu
    object abelas1: TMenuItem [2]
      Caption = '&Tables'
      object Regions1: TMenuItem
        Action = ACTlistRegion
      end
      object CustomerMarket1: TMenuItem
        Action = ACTlistCustMarket
      end
      object ConsigneeMarket1: TMenuItem
        Action = ACTlistConsMarket
      end
      object CountryCalendar1: TMenuItem
        Action = ACTlistCountryCal
      end
      object States1: TMenuItem
        Action = ACTlistState
      end
      object PaymentCodes1: TMenuItem
        Action = ACTlistPaymentCode
      end
      object Carrier1: TMenuItem
        Action = ACTlistAgents
      end
      object WarehouseandPlatforms1: TMenuItem
        Action = ACTlistWhsePlat
      end
      object Carriers1: TMenuItem
        Action = ACTlistCarrier
      end
      object Customers1: TMenuItem
        Action = ACTlistCustomer
      end
      object Consignee1: TMenuItem
        Action = ACTlistConsignee
      end
      object BusinessEntities1: TMenuItem
        Action = ACTlistEntityLink
      end
      object CustomerGroup1: TMenuItem
        Action = ACTcustomerGroup
      end
      object MNUlistBankAccounts: TMenuItem
        Action = ACTlistBankAccounts
      end
      object DocsCheckList1: TMenuItem
        Action = ACTlistDocsCheck
      end
      object DocsCheckDefaults1: TMenuItem
        Action = ACTlistDocsCheckDefaults
      end
      object ACTlistIKAM1: TMenuItem
        Action = ACTlistIKAM
      end
      object MNIemailTpl: TMenuItem
        Action = ACTlistEmailTpl
        Enabled = False
        Visible = False
      end
      object MNIlistCustomer: TMenuItem
        Action = ACTlistCustomerLists
      end
      object MNIlistDocsCheckListRules: TMenuItem
        Action = ACTlistDocsCheckListRules
      end
      object SalesRegion1: TMenuItem
        Action = ACTListSalesRegion
      end
    end
    object Offices1: TMenuItem [3]
      Caption = '&Offices'
      object SalesDirection1: TMenuItem
        Action = ACTlistSalesDir
      end
      object SalesOffices1: TMenuItem
        Action = ACTSalesOffices
      end
      object SalesManager1: TMenuItem
        Action = ACTlistSalesMan
      end
      object SalesAssistants2: TMenuItem
        Action = ACTlistSalesAssist
      end
      object BackOffice1: TMenuItem
        Action = ACTlistBackOffice
      end
      object BackOfficeAssistant1: TMenuItem
        Action = ACTlistBackAssist
      end
      object mnilistCustSalesAssist: TMenuItem
        Action = ACTlistCustSalesAssist
      end
    end
    object BusinessUnit1: TMenuItem [4]
      Caption = 'Business Unit'
      object ChangeBusinessUnit1: TMenuItem
        Action = ACTchangeBusinessUnit
      end
    end
  end
  inherited APEapplicationEvent: TApplicationEvents
    Left = 192
    Top = 8
  end
  inherited PRVprivileges: TPrivileges
    Left = 160
    Top = 8
  end
  object ACLmain: TActionList
    Images = IMLmain
    Left = 125
    Top = 5
    object ACTrep: TAction
      Category = 'Reports'
      Caption = '&xxx Report...'
    end
    object ACTlistRegion: TAction
      Tag = 1
      Category = 'Tables'
      Caption = '&Region'
      Hint = 'Region'
      OnExecute = ACTlistRegionExecute
    end
    object ACTlistCustMarket: TAction
      Tag = 2
      Category = 'Tables'
      Caption = 'C&ustomer Market'
      Hint = 'Customer Market'
      OnExecute = ACTlistCustMarketExecute
    end
    object ACTlistConsMarket: TAction
      Tag = 3
      Category = 'Tables'
      Caption = 'C&onsignee Market'
      Hint = 'Consignee Market'
      OnExecute = ACTlistConsMarketExecute
    end
    object ACTlistCountryCal: TAction
      Tag = 4
      Category = 'Tables'
      Caption = 'Cou&ntry Calendar'
      Hint = 'Country Calendar'
      OnExecute = ACTlistCountryCalExecute
    end
    object ACTlistState: TAction
      Tag = 5
      Category = 'Tables'
      Caption = '&State'
      Hint = 'State'
      OnExecute = ACTlistStateExecute
    end
    object ACTlistPaymentCode: TAction
      Tag = 6
      Category = 'Tables'
      Caption = '&Payment Code'
      Hint = 'Payment Code'
      OnExecute = ACTlistPaymentCodeExecute
    end
    object ACTlistAgents: TAction
      Tag = 7
      Category = 'Tables'
      Caption = '&Agent'
      Hint = 'Agent'
      OnExecute = ACTlistAgentsExecute
    end
    object ACTlistWhsePlat: TAction
      Tag = 8
      Category = 'Tables'
      Caption = '&Warehouse and Platform'
      Hint = 'Warehouse and Platform'
      OnExecute = ACTlistWhsePlatExecute
    end
    object ACTlistCarrier: TAction
      Tag = 9
      Category = 'Tables'
      Caption = '&Carrier'
      Hint = 'Carrier'
      OnExecute = ACTlistCarrierExecute
    end
    object ACTlistCustomer: TAction
      Tag = 10
      Category = 'Tables'
      Caption = 'Cus&tomer'
      Hint = 'Customer'
      OnExecute = ACTlistCustomerExecute
    end
    object ACTlistConsignee: TAction
      Tag = 11
      Category = 'Tables'
      Caption = 'Consi&gnee'
      Hint = 'Consignee'
      OnExecute = ACTlistConsigneeExecute
    end
    object ACTlistSalesAssist: TAction
      Tag = 91
      Category = 'Offices'
      Caption = 'Sales Assistants'
      Hint = 'Sales Assistants Management'
      OnExecute = ACTlistSalesAssistExecute
    end
    object ACTlistSalesMan: TAction
      Tag = 12
      Category = 'Offices'
      Caption = 'Sales Manager'
      Hint = 'Sales Manager Management'
      OnExecute = ACTlistSalesManExecute
    end
    object ACTlistEntityLink: TAction
      Tag = 30
      Category = 'Tables'
      Caption = 'eBusiness Entities'
      Hint = 'eBusiness Entities'
      OnExecute = ACTlistEntityLinkExecute
    end
    object ACTlistSalesDir: TAction
      Tag = 31
      Category = 'Offices'
      Caption = 'Sales Direction'
      Hint = 'Sales Direction Management'
      OnExecute = ACTlistSalesDirExecute
    end
    object ACTSalesOffices: TAction
      Tag = 32
      Category = 'Offices'
      Caption = 'Sales Offices'
      Hint = 'Sales Offices Management'
      OnExecute = ACTSalesOfficesExecute
    end
    object ACTcustomerGroup: TAction
      Tag = 33
      Category = 'Tables'
      Caption = 'Customer Group'
      Hint = 'Customer Group Management'
      OnExecute = ACTcustomerGroupExecute
    end
    object ACTlistBankAccounts: TAction
      Tag = 34
      Category = 'Tables'
      Caption = 'Bank Accounts'
      Hint = 'Bank Accounts Management'
      OnExecute = ACTlistBankAccountsExecute
    end
    object ACTlistDocsCheck: TAction
      Tag = 35
      Category = 'Tables'
      Caption = 'Docs Check List'
      Hint = 'Documents Check List'
      OnExecute = ACTlistDocsCheckExecute
    end
    object ACTlistDocsCheckDefaults: TAction
      Tag = 36
      Category = 'Tables'
      Caption = 'Docs Check Defaults'
      Hint = 'Docs Check List Defaults'
      OnExecute = ACTlistDocsCheckDefaultsExecute
    end
    object ACTlistIKAM: TAction
      Tag = 37
      Category = 'Tables'
      Caption = 'IKAM'
      Hint = 'Ikam'
      OnExecute = ACTlistIKAMExecute
    end
    object ACTlistBackOffice: TAction
      Tag = 38
      Category = 'Offices'
      Caption = 'Back Offices'
      Hint = 'Back Offices Management'
      OnExecute = ACTlistBackOfficeExecute
    end
    object ACTlistBackAssist: TAction
      Tag = 39
      Category = 'Offices'
      Caption = 'Back Office Assistants'
      Hint = 'Back Office Assistants Management'
      OnExecute = ACTlistBackAssistExecute
    end
    object ACTlistCustSalesAssist: TAction
      Tag = 40
      Category = 'Tables'
      Caption = 'Customer Sales Assistants'
      Hint = 'Customer Sales Assistants Management'
      OnExecute = ACTlistCustSalesAssistExecute
    end
    object ACTlistEmailTpl: TAction
      Category = 'Tables'
      Caption = 'Email Template'
      OnExecute = ACTlistEmailTplExecute
    end
    object ACTlistCustomerLists: TAction
      Tag = 41
      Category = 'Tables'
      Caption = 'Customer Lists'
      Hint = 'Customer Lists '
      OnExecute = ACTlistCustomerListsExecute
    end
    object ACTlistDocsCheckListRules: TAction
      Tag = 42
      Category = 'Tables'
      Caption = 'Docs Check List Rules'
      Hint = 'Documents Check List Rules'
      OnExecute = ACTlistDocsCheckListRulesExecute
    end
    object ACTListSalesRegion: TAction
      Tag = 13
      Category = 'Tables'
      Caption = 'Sales Region'
      Hint = 'Sales Region Maintenance'
      OnExecute = ACTListSalesRegionExecute
    end
    object ACTchangeBusinessUnit: TAction
      Tag = 43
      Category = 'BusinessUnit'
      Caption = 'Change Business Unit'
      OnExecute = ACTchangeBusinessUnitExecute
    end
  end
end

```

<!-- tabs:end -->
